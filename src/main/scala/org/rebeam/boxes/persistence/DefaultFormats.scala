package org.rebeam.boxes.persistence

import java.lang.reflect.Modifier

import scala.collection.mutable.ListBuffer

import boxes.transact.{Txn, Box}

import scala.reflect.ClassTag
import scala.util.control.NonFatal

object PrimFormats {
  implicit val booleanFormat = new Format[Boolean] {
    def write(s: Boolean, c: WriteContext) = c.writer.write(BooleanToken(s))
    def read(c: ReadContext) = c.reader.pullBoolean()
  }
  implicit val intFormat = new Format[Int] {
    def write(s: Int, c: WriteContext) = c.writer.write(IntToken(s))
    def read(c: ReadContext) = c.reader.pullInt()
  }
  implicit val longFormat = new Format[Long] {
    def write(s: Long, c: WriteContext) = c.writer.write(LongToken(s))
    def read(c: ReadContext) = c.reader.pullLong()
  }
  implicit val floatFormat = new Format[Float] {
    def write(s: Float, c: WriteContext) = c.writer.write(FloatToken(s))
    def read(c: ReadContext) = c.reader.pullFloat()
  }
  implicit val doubleFormat = new Format[Double] {
    def write(s: Double, c: WriteContext) = c.writer.write(DoubleToken(s))
    def read(c: ReadContext) = c.reader.pullDouble()
  }
  implicit val stringFormat = new Format[String] {
    def write(s: String, c: WriteContext) = c.writer.write(StringToken(s))
    def read(c: ReadContext) = c.reader.pullString()
  }
}

object TokenUtils {
  def readDict(c: ReadContext): Unit = {

  }
}

//See http://stackoverflow.com/questions/4403906/is-it-possible-in-scala-to-force-the-caller-to-specify-a-type-parameter-for-a-po for
//an explanation of why we structure implicits this way.
//As a brief summary, we want the compiler to prefer the most specific implicits (e.g. the Writes[Map[String, V]] function should be
//preferred to the Writes[Map[K, V]] one since it only exists to make this specific case neater). Hence we define the fallback general
//Writes and Reads here, and the more specific ones later.
trait LowPriorityCollectionFormats {

  //This is the lower-priority format for maps, works in all cases but needs to use more boiler-plate in representation of non-string keys.
  implicit def mapFormat[K, V](implicit writesK: Writes[K], writesV: Writes[V], readsK: Reads[K], readsV: Reads[V]): Format[Map[K, V]] = new Format[Map[K, V]] {
    def write(map: Map[K, V], writeContext: WriteContext) = writesMap[K, V].write(map, writeContext)
    def read(readContext: ReadContext) = readsMap[K, V].read(readContext)
  }

  implicit def writesMap[K, V](implicit writesK: Writes[K], writesV: Writes[V]): Writes[Map[K, V]] = new Writes[Map[K, V]] {
    def write(map: Map[K, V], c: WriteContext): Unit = {
      c.writer.write(OpenArr(PresentationName("Map")))
      map.foreach(entry => {
        c.writer.write(OpenDict(PresentationName("Entry")))
        c.writer.write(DictEntry("key"))
        writesK.write(entry._1, c)
        c.writer.write(DictEntry("value"))
        writesV.write(entry._2, c)
        c.writer.write(CloseDict)
      })
      c.writer.write(CloseArr)
    }
  }

  implicit def readsMap[K, V](implicit readsK: Reads[K], readsV: Reads[V]): Reads[Map[K, V]] = new Reads[Map[K, V]] {

    def readKeyOrValue(c: ReadContext): Either[K, V] = {
      c.reader.pull() match {
        case DictEntry("key", LinkEmpty) => Left(readsK.read(c))
        case DictEntry("value", LinkEmpty) => Right(readsV.read(c))
        case _ => throw new IncorrectTokenException("Expected DictEntry(value, LinkEmpty) in Map[_, _] entry dict")
      }
    }

    def readKeyAndValue(c: ReadContext): (K, V) = {
      (readKeyOrValue(c), readKeyOrValue(c)) match {
        case (Left(key), Right(value)) => (key, value)
        case (Right(value), Left(key)) => (key, value)
        case _ => throw new IncorrectTokenException("Expected dictionary of key and value in Map[_, _] entry")
      }
    }

    def read(c: ReadContext) = {

      c.reader.pull() match {
        case OpenArr(_) =>
          val entries = ListBuffer[(K, V)]()
          while (c.reader.peek != CloseArr) {

            entries.append(c.reader.pull() match {
              case OpenDict(_, _) =>
                val entry = readKeyAndValue(c)
                c.reader.pullAndAssertEquals(CloseDict)
                entry
              case _ => throw new IncorrectTokenException("Expected OpenDict for each Map[_, _] entry")
            })

          }
          c.reader.pullAndAssertEquals(CloseArr)
          Map(entries: _*)
        case _ => throw new IncorrectTokenException("Expected OpenArr at start of Map[_, _]")
      }

    }
  }
}

object CollectionFormats extends LowPriorityCollectionFormats {
  implicit def writesList[T](implicit writes: Writes[T]): Writes[List[T]] = new Writes[List[T]] {
    def write(list: List[T], c: WriteContext) {
      c.writer.write(OpenArr(PresentationName("List")))
      list.foreach(t => writes.write(t, c))
      c.writer.write(CloseArr)
    }
  }
  implicit def readsList[T](implicit reads: Reads[T]): Reads[List[T]] = new Reads[List[T]] {
    def read(c: ReadContext) = {
      val b = ListBuffer[T]()
      c.reader.pull() match {
        case OpenArr(_) =>
          while (c.reader.peek != CloseArr) {
            b.append(reads.read(c))
          }
          c.reader.pullAndAssertEquals(CloseArr)
          b.toList

        case _ => throw new IncorrectTokenException("Expected OpenArr at start of List[_]")
      }
    }
  }
  implicit def listFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[List[T]] = new Format[List[T]] {
    override def read(context: ReadContext): List[T] = readsList[T].read(context)
    override def write(obj: List[T], context: WriteContext): Unit = writesList[T].write(obj, context)
  }

  implicit def writesSet[T](implicit writes: Writes[T]): Writes[Set[T]] = new Writes[Set[T]] {
    def write(list: Set[T], c: WriteContext) {
      c.writer.write(OpenArr(PresentationName("Set")))
      list.foreach(t => writes.write(t, c))
      c.writer.write(CloseArr)
    }
  }
  implicit def readsSet[T](implicit reads: Reads[T]): Reads[Set[T]] = new Reads[Set[T]] {
    def read(c: ReadContext) = {
      val b = ListBuffer[T]()
      c.reader.pull() match {
        case OpenArr(_) =>
          while (c.reader.peek != CloseArr) {
            b.append(reads.read(c))
          }
          c.reader.pullAndAssertEquals(CloseArr)
          b.toSet

        case _ => throw new IncorrectTokenException("Expected OpenArr at start of Set[_]")
      }
    }
  }
  implicit def setFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Set[T]] = new Format[Set[T]] {
    override def read(context: ReadContext): Set[T] = readsSet[T].read(context)
    override def write(obj: Set[T], context: WriteContext): Unit = writesSet[T].write(obj, context)
  }

  //This is the higher-priority writer for Maps that have string keys, it can use a more compact representation
  //that should also be more idiomatic in e.g. JSON and XML
  implicit def writesStringKeyedMap[V](implicit writes: Writes[V]): Writes[Map[String, V]] = new Writes[Map[String, V]] {
    def write(map: Map[String, V], c: WriteContext): Unit = {
      c.writer.write(OpenDict(PresentationName("Map")))
      map.foreach(entry => {
        c.writer.write(DictEntry(entry._1))
        writes.write(entry._2, c)
      })
      c.writer.write(CloseDict)
    }
  }

  implicit def readsStringKeyedMap[V](implicit reads: Reads[V]): Reads[Map[String, V]] = new Reads[Map[String, V]] {
    def read(c: ReadContext) = {
      c.reader.pull() match {
        case OpenDict(_, _) =>
          val entries = ListBuffer[(String, V)]()
          while (c.reader.peek != CloseDict) {
            entries.append(c.reader.pull() match {
              case DictEntry(key, LinkEmpty) => {
                val value = reads.read(c)
                (key, value)
              }
              case _ => throw new IncorrectTokenException("Expected DictEntry(key, LinkEmpty) at start of Map[String, _] entry")
            })
          }
          c.reader.pullAndAssertEquals(CloseDict)
          Map(entries: _*)

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }
  }

  implicit def stringKeyedMapFormat[V](implicit writesV: Writes[V], readsV: Reads[V]): Format[Map[String, V]] = new Format[Map[String, V]] {
    def write(map: Map[String, V], writeContext: WriteContext) = writesStringKeyedMap[V].write(map, writeContext)
    def read(readContext: ReadContext) = readsStringKeyedMap[V].read(readContext)
  }


}

object BasicFormats {
  implicit def writesOption[T](implicit writes: Writes[T]): Writes[Option[T]] = new Writes[Option[T]] {
    def write(option: Option[T], c: WriteContext) {
      option match {
        case Some(v) => writes.write(v, c)
        case None => c.writer.write(NoneToken)
      }
    }
  }
  implicit def readsOption[T](implicit reads: Reads[T]): Reads[Option[T]] = new Reads[Option[T]] {
    def read(c: ReadContext) = {
      c.reader.peek match {
        case NoneToken =>
          c.reader.pullAndAssertEquals(NoneToken)
          None

        case _ => Some(reads.read(c))
      }
    }
  }
}

private object BoxFormatUtils {
  def write[T](box: Box[T], c: WriteContext, linkStrategy: LinkStrategy, writes: Writes[T]) {
    val id = box.id()
    linkStrategy match {

      case UseLinks =>
        if (c.writer.isBoxCached(id)) {
          c.writer.write(BoxToken(LinkRef(id)))
        } else {
          c.writer.write(BoxToken(LinkId(id)))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

      case NoLinks =>
        c.writer.write(BoxToken(LinkEmpty))
        writes.write(box.get()(c.txn), c)

      case NoLinksOrDuplicates =>
        if (c.writer.isBoxCached(id)) {
          throw new BoxCacheException("Box id " + id + " was already cached, but boxLinkStrategy is NoLinksOrDuplicates")
        } else {
          c.writer.write(BoxToken(LinkEmpty))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

    }
  }

  def read[T](c: ReadContext, linkStrategy: LinkStrategy, reads: Reads[T]) = {
    c.reader.pull() match {
      case BoxToken(link) =>
        link match {
          case LinkEmpty => c.txn.create(reads.read(c))
          case LinkId(id) =>
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkId (" + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            val box = c.txn.create(reads.read(c))
            c.reader.putBox(id, box)
            box

          case LinkRef(id) =>
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkRef( " + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            c.reader.getBox(id).asInstanceOf[Box[T]]
        }

      case _ => throw new IncorrectTokenException("Expected BoxToken at start of Box[_]")
    }
  }
}

object BoxFormatsNoLinksOrDuplicates {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinksOrDuplicates, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinksOrDuplicates, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinksOrDuplicates, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinksOrDuplicates, reads)
  }
}

object BoxFormatsNoLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinks, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinks, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinks, reads)
  }
}

object BoxFormatsUseLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, UseLinks, writes)
  }

  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, UseLinks, reads)
  }

  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, UseLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, UseLinks, reads)
  }
}

//FIXME add links for dicts used in ProductFormats and NodeFormats, with a LinkStrategy.

object ProductFormats {

  private def writeDictEntry[T :Format](p: Product, name: String, index: Int, c: WriteContext): Unit = {
    c.writer.write(DictEntry(name))
    implicitly[Format[T]].write(p.productElement(index).asInstanceOf[T], c)
  }

  private def readDictEntry[T :Format](name: String, c: ReadContext): T = {
    c.reader.pullAndAssertEquals(DictEntry(name, LinkEmpty))
    implicitly[Format[T]].read(c)
  }

  def productFormat2[P1: Format, P2: Format, P <: Product :ClassTag](construct: (P1, P2) => P)(name1: String, name2: String)(name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>
          val n = construct(
            readDictEntry[P1](name1, c),
            readDictEntry[P2](name2, c)
          )
          c.reader.pullAndAssertEquals(CloseDict)
          n

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }
}

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

  private def writeDictEntry[T :Format](n: Product, name: String, index: Int, c: WriteContext, linkStrategy: LinkStrategy): Unit = {
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
        case UseLinks => LinkId(id)
        case NoLinksOrDuplicates => LinkEmpty
        case NoLinks => LinkEmpty             //We always enforce no duplicates anyway
      }

      //Open an entry for the Box
      c.writer.write(DictEntry(name, link))

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
      case LinkEmpty => {}                            //No cache stuff to do
      case LinkId(id) => c.reader.putBox(id, box)     //Cache our box for anything using it later in stream
      case LinkRef(id) => throw new IncorrectTokenException("DictEntry must NOT have a LinkRef in a Node Dict, found ref to " + id)
    }
    box.set(implicitly[Format[T]].read(c))
  }

  def nodeFormat2[P1: Format, P2: Format, N <: Product :ClassTag](construct: (Box[P1], Box[P2]) => N, default: (Txn) => N)(name1: String, name2: String)(name: TokenName = NoName, linkStrategy: LinkStrategy = NoLinks) : Format[N] = new Format[N] {

    def write(n: N, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](n, name1, 0, c, linkStrategy)
      writeDictEntry[P2](n, name2, 1, c, linkStrategy)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): N = {
      implicit val txn = c.txn
      c.reader.pull() match {
        case OpenDict(_, _) =>
          val n = default(txn)

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(fieldName, link) => fieldName match {
                case s if s == name1 => useDictEntry[P1](n, 0, c, link)
                case s if s == name2 => useDictEntry[P2](n, 1, c, link)
                case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
              }
              case x: Token => throw new IncorrectTokenException("Expected only DictEntry's in a Node Dict, got " + x)
            }
          }

          c.reader.pullAndAssertEquals(CloseDict)
          n

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }

  def nodeFormat4[P1: Format, P2: Format, P3: Format, P4: Format, N <: Product :ClassTag](construct: (Box[P1], Box[P2], Box[P3], Box[P4]) => N, default: (Txn) => N)(name1: String, name2: String, name3: String, name4: String)(name: TokenName = NoName, linkStrategy: LinkStrategy = NoLinks) : Format[N] = new Format[N] {

    def write(n: N, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](n, name1, 0, c, linkStrategy)
      writeDictEntry[P2](n, name2, 1, c, linkStrategy)
      writeDictEntry[P3](n, name3, 2, c, linkStrategy)
      writeDictEntry[P4](n, name4, 3, c, linkStrategy)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): N = {
      implicit val txn = c.txn
      c.reader.pull() match {
        case OpenDict(_, _) =>
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
              case x: Token => throw new IncorrectTokenException("Expected only DictEntry's in a Node Dict, got " + x)
            }
          }

          c.reader.pullAndAssertEquals(CloseDict)
          n

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


}