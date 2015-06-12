package org.rebeam.boxes.persistence

import boxes.transact.{Txn, Box}

/**
 * These formats are very similar to ProductFormats, with the important difference that they expect to read/write
 * case classes (Products) of Boxes, which we call "Nodes" for example case class Person(name: Box[String], age: Box[Int]).
 * We refer to the Node's class as N below. Writes as a Dict with an entry for each box, containing the value of the box.
 * The link in the DictEntry itself is used to provide an id for the Box if required, BoxTokens are not used since they
 * are redundant. This means that a Node containing Boxes of types B1, B2 etc. will be serialised similarly to a Product
 * containing unboxed values of B1, B2 etc. Refs are never used.
 *
 * There are some additional requirements/features when using these formats:
 *  1. When reading, the formats always create the instance of N via a provided default method of type (Txn) => N,
 *  which is expected to create a "default" case class with entirely new boxes.
 *  2. When reading, it is acceptable for some fields to be missing - they are left as defaults. The reading process
 *  simply creates a default N, then sets any boxes that are present in the tokens. This allows for transparent upgrading
 *  of data from a previous version of the class with fewer fields.
 *  3. When writing, boxes are all required NOT to be in the cache when written - that is, they are not referenced anywhere
 *  in the token stream leading up to the moment of writing. This means that we can guarantee it is acceptable to create
 *  a new Box when reading again.
 *
 *  By requiring (and enforcing) that Nodes use their own boxes and these boxes are not used in any other Nodes, we can
 *  provide for transparent upgrading of nodes by using default values. In addition, the provided default method of type
 *  (Txn) => N can create any required reactions when the nodes are created, providing an easy means of handling
 *  serialisation and deserialisation of reactions.
 */
object NodeFormats {

  private def writeDictEntry[T :Format](n: Product, name: String, index: Int, c: WriteContext, linkStrategy: NoDuplicatesLinkStrategy): Unit = {
    implicit val txn = c.txn

    val box = n.productElement(index).asInstanceOf[Box[T]]
    val id = box.id()

    //NodeFormat will not use link references, but can use ids, for example for sharing box id with clients on network.
    //This is done by caching the box and using the id, but throwing an exception if we find the box is already cached.
    //This ensures that only one copy of the Box is in use in a Node, so that the Node technique of recreating Nodes
    //from default with new boxes is valid. Note that we don't really care if other Formats end up referencing our
    //Boxes later - we add them to cache when reading.
    if (c.writer.isBoxCached(id)) {
      throw new BoxCacheException("Box id " + id + " was already cached, but NodeFormats doesn't work with multiply-referenced Boxes")
    } else {

      val link = linkStrategy match {
        case IdLinks => LinkId(id)
        case EmptyLinks => LinkEmpty
      }

      //Open an entry for the Box
      c.writer.write(DictEntry(name, link))

      //Cache the box whether we used LinkId or LinkEmpty - we use this to avoid duplicates in either case
      c.writer.cacheBox(id)
      implicitly[Format[T]].write(box.get(), c)
    }
  }

  private def useDictEntry[T :Format](n: Product, index: Int, c: ReadContext, link: Link): Unit = {
    implicit val txn = c.txn
    val box = n.productElement(index).asInstanceOf[Box[T]]

    //We accept LinkEmpty, with nothing to do, or LinkId, in which case we putBox in case of any later references.
    //We do NOT accept LinkRef, since we never write one.
    link match {
      case LinkEmpty =>                               //No cache stuff to do
      case LinkId(id) => c.reader.putBox(id, box)     //Cache our box for anything using it later in stream
      case LinkRef(id) => throw new IncorrectTokenException("DictEntry must NOT have a LinkRef in a Node Dict, found ref to " + id)
    }
    box.set(implicitly[Format[T]].read(c))
  }

  private def writeNode[N](n: N, c: WriteContext, name: TokenName, nodeLinkStrategy: LinkStrategy, writeEntriesAndClose: (N, WriteContext) => Unit): Unit = {
    nodeLinkStrategy match {
      case AllLinks =>
        c.writer.cache(n) match {
          case Cached(id) => c.writer.write(OpenDict(name, LinkRef(id)))
          case New(id) =>
            c.writer.write(OpenDict(name, LinkId(id)))
            writeEntriesAndClose(n, c)
        }

      case IdLinks =>
        c.writer.cache(n) match {
          case Cached(id) => throw new NodeCacheException("Node " + n + " was already cached, but nodeLinkStrategy is " + nodeLinkStrategy)
          case New(id) =>
            c.writer.write(OpenDict(name, LinkId(id)))
            writeEntriesAndClose(n, c)
        }

      case EmptyLinks =>
        c.writer.cache(n) match {
          case Cached(id) => throw new NodeCacheException("Node " + n + " was already cached, but nodeLinkStrategy is " + nodeLinkStrategy)
          case New(id) =>
            c.writer.write(OpenDict(name, LinkEmpty))
            writeEntriesAndClose(n, c)
        }
    }
  }

  private def readNode[N](c: ReadContext, readEntriesAndClose: (ReadContext) => N): N = {
    c.reader.pull() match {
      case OpenDict(name, LinkEmpty) => readEntriesAndClose(c)
      case OpenDict(name, LinkRef(id)) => c.reader.getCache(id).asInstanceOf[N]
      case OpenDict(name, LinkId(id)) =>
        val n = readEntriesAndClose(c)
        c.reader.putCache(id, n)
        n

      case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
    }
  }


  def nodeFormat1[P1: Format, N <: Product](construct: (Box[P1]) => N, default: (Txn) => N)
                                           (name1: String)
                                           (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat2[P1: Format, P2: Format, N <: Product](construct: (Box[P1], Box[P2]) => N, default: (Txn) => N)
                                                       (name1: String, name2: String)
                                                       (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat3[P1: Format, P2: Format, P3: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3]) => N, default: (Txn) => N)
                                                                   (name1: String, name2: String, name3: String)
                                                                   (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat4[P1: Format, P2: Format, P3: Format, P4: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4]) => N, default: (Txn) => N)
                                                                               (name1: String, name2: String, name3: String, name4: String)
                                                                               (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat5[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5]) => N, default: (Txn) => N)
                                                                                           (name1: String, name2: String, name3: String, name4: String, name5: String)
                                                                                           (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat6[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6]) => N, default: (Txn) => N)
                                                                                                       (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String)
                                                                                                       (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat7[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7]) => N, default: (Txn) => N)
                                                                                                                   (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String)
                                                                                                                   (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat8[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8]) => N, default: (Txn) => N)
                                                                                                                               (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String)
                                                                                                                               (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat9[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9]) => N, default: (Txn) => N)
                                                                                                                                           (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String)
                                                                                                                                           (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat10[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10]) => N, default: (Txn) => N)
                                                                                                                                                         (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String)
                                                                                                                                                         (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat11[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11]) => N, default: (Txn) => N)
                                                                                                                                                                      (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String)
                                                                                                                                                                      (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat12[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12]) => N, default: (Txn) => N)
                                                                                                                                                                                   (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String)
                                                                                                                                                                                   (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat13[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13]) => N, default: (Txn) => N)
                                                                                                                                                                                                (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String)
                                                                                                                                                                                                (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat14[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14]) => N, default: (Txn) => N)
                                                                                                                                                                                                             (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String)
                                                                                                                                                                                                             (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat15[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15]) => N, default: (Txn) => N)
                                                                                                                                                                                                                          (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String)
                                                                                                                                                                                                                          (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat16[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                       (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String)
                                                                                                                                                                                                                                       (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat17[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                    (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String)
                                                                                                                                                                                                                                                    (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat18[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17], Box[P18]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                                 (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String)
                                                                                                                                                                                                                                                                 (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      writeDictEntry[P18](n, name18, 17, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case s if s == name18 => useDictEntry[P18](n, 17, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat19[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17], Box[P18], Box[P19]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                                              (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String)
                                                                                                                                                                                                                                                                              (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      writeDictEntry[P18](n, name18, 17, c, boxLinkStrategy)
      writeDictEntry[P19](n, name19, 18, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case s if s == name18 => useDictEntry[P18](n, 17, c, link)
            case s if s == name19 => useDictEntry[P19](n, 18, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat20[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17], Box[P18], Box[P19], Box[P20]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                                                           (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String)
                                                                                                                                                                                                                                                                                           (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      writeDictEntry[P18](n, name18, 17, c, boxLinkStrategy)
      writeDictEntry[P19](n, name19, 18, c, boxLinkStrategy)
      writeDictEntry[P20](n, name20, 19, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case s if s == name18 => useDictEntry[P18](n, 17, c, link)
            case s if s == name19 => useDictEntry[P19](n, 18, c, link)
            case s if s == name20 => useDictEntry[P20](n, 19, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat21[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, P21: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17], Box[P18], Box[P19], Box[P20], Box[P21]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                                                                        (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String, name21: String)
                                                                                                                                                                                                                                                                                                        (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      writeDictEntry[P18](n, name18, 17, c, boxLinkStrategy)
      writeDictEntry[P19](n, name19, 18, c, boxLinkStrategy)
      writeDictEntry[P20](n, name20, 19, c, boxLinkStrategy)
      writeDictEntry[P21](n, name21, 20, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case s if s == name18 => useDictEntry[P18](n, 17, c, link)
            case s if s == name19 => useDictEntry[P19](n, 18, c, link)
            case s if s == name20 => useDictEntry[P20](n, 19, c, link)
            case s if s == name21 => useDictEntry[P21](n, 20, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


  def nodeFormat22[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, P21: Format, P22: Format, N <: Product](construct: (Box[P1], Box[P2], Box[P3], Box[P4], Box[P5], Box[P6], Box[P7], Box[P8], Box[P9], Box[P10], Box[P11], Box[P12], Box[P13], Box[P14], Box[P15], Box[P16], Box[P17], Box[P18], Box[P19], Box[P20], Box[P21], Box[P22]) => N, default: (Txn) => N)
                                                                                                                                                                                                                                                                                                                     (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String, name21: String, name22: String)
                                                                                                                                                                                                                                                                                                                     (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {

    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)
      writeDictEntry[P2](n, name2, 1, c, boxLinkStrategy)
      writeDictEntry[P3](n, name3, 2, c, boxLinkStrategy)
      writeDictEntry[P4](n, name4, 3, c, boxLinkStrategy)
      writeDictEntry[P5](n, name5, 4, c, boxLinkStrategy)
      writeDictEntry[P6](n, name6, 5, c, boxLinkStrategy)
      writeDictEntry[P7](n, name7, 6, c, boxLinkStrategy)
      writeDictEntry[P8](n, name8, 7, c, boxLinkStrategy)
      writeDictEntry[P9](n, name9, 8, c, boxLinkStrategy)
      writeDictEntry[P10](n, name10, 9, c, boxLinkStrategy)
      writeDictEntry[P11](n, name11, 10, c, boxLinkStrategy)
      writeDictEntry[P12](n, name12, 11, c, boxLinkStrategy)
      writeDictEntry[P13](n, name13, 12, c, boxLinkStrategy)
      writeDictEntry[P14](n, name14, 13, c, boxLinkStrategy)
      writeDictEntry[P15](n, name15, 14, c, boxLinkStrategy)
      writeDictEntry[P16](n, name16, 15, c, boxLinkStrategy)
      writeDictEntry[P17](n, name17, 16, c, boxLinkStrategy)
      writeDictEntry[P18](n, name18, 17, c, boxLinkStrategy)
      writeDictEntry[P19](n, name19, 18, c, boxLinkStrategy)
      writeDictEntry[P20](n, name20, 19, c, boxLinkStrategy)
      writeDictEntry[P21](n, name21, 20, c, boxLinkStrategy)
      writeDictEntry[P22](n, name22, 21, c, boxLinkStrategy)
      c.writer.write(CloseDict)
    }

    def readEntriesAndClose(c: ReadContext): N = {
      implicit val txn = c.txn
      val n = default(txn)

      while (c.reader.peek != CloseDict) {
        c.reader.pull() match {
          case DictEntry(fieldName, link) => fieldName match {
            case s if s == name1 => useDictEntry[P1](n, 0, c, link)
            case s if s == name2 => useDictEntry[P2](n, 1, c, link)
            case s if s == name3 => useDictEntry[P3](n, 2, c, link)
            case s if s == name4 => useDictEntry[P4](n, 3, c, link)
            case s if s == name5 => useDictEntry[P5](n, 4, c, link)
            case s if s == name6 => useDictEntry[P6](n, 5, c, link)
            case s if s == name7 => useDictEntry[P7](n, 6, c, link)
            case s if s == name8 => useDictEntry[P8](n, 7, c, link)
            case s if s == name9 => useDictEntry[P9](n, 8, c, link)
            case s if s == name10 => useDictEntry[P10](n, 9, c, link)
            case s if s == name11 => useDictEntry[P11](n, 10, c, link)
            case s if s == name12 => useDictEntry[P12](n, 11, c, link)
            case s if s == name13 => useDictEntry[P13](n, 12, c, link)
            case s if s == name14 => useDictEntry[P14](n, 13, c, link)
            case s if s == name15 => useDictEntry[P15](n, 14, c, link)
            case s if s == name16 => useDictEntry[P16](n, 15, c, link)
            case s if s == name17 => useDictEntry[P17](n, 16, c, link)
            case s if s == name18 => useDictEntry[P18](n, 17, c, link)
            case s if s == name19 => useDictEntry[P19](n, 18, c, link)
            case s if s == name20 => useDictEntry[P20](n, 19, c, link)
            case s if s == name21 => useDictEntry[P21](n, 20, c, link)
            case s if s == name22 => useDictEntry[P22](n, 21, c, link)
            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
          }
          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
        }
      }

      c.reader.pullAndAssertEquals(CloseDict)
      n
    }

    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)

  }


}