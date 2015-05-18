package org.rebeam.boxes.persistence

import scala.collection.mutable.ListBuffer

import boxes.transact.Box

object PrimFormats {
  implicit val booleanFormat = new Format[Boolean] {
    def write(s: Boolean, c: WriteContext) = c.writer.write(BooleanToken(s))
    def read(context: ReadContext) = context.reader.pullBoolean()
  }
  implicit val intFormat = new Format[Int] {
    def write(s: Int, c: WriteContext) = c.writer.write(IntToken(s))
    def read(context: ReadContext) = context.reader.pullInt()
  }
  implicit val longFormat = new Format[Long] {
    def write(s: Long, c: WriteContext) = c.writer.write(LongToken(s))
    def read(context: ReadContext) = context.reader.pullLong()
  }
  implicit val floatFormat = new Format[Float] {
    def write(s: Float, c: WriteContext) = c.writer.write(FloatToken(s))
    def read(context: ReadContext) = context.reader.pullFloat()
  }
  implicit val doubleFormat = new Format[Double] {
    def write(s: Double, c: WriteContext) = c.writer.write(DoubleToken(s))
    def read(context: ReadContext) = context.reader.pullDouble()
  }
  implicit val stringFormat = new Format[String] {
    def write(s: String, c: WriteContext) = c.writer.write(StringToken(s))
    def read(context: ReadContext) = context.reader.pullString()
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

  //This is the lower-priority writer for maps, works in all cases but needs to use more boiler-plate in representation of non-string keys.
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
        case DictEntry("key") => Left(readsK.read(c))
        case DictEntry("value") => Right(readsV.read(c))
        case _ => throw new IncorrectTokenException("Expected DictEntry(value) in Map[_, _] entry dict")
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
        case OpenArr(_) => {
          val entries = ListBuffer[(K, V)]()
          while (c.reader.peek != CloseArr) {

            entries.append(c.reader.pull() match {
              case OpenDict(_, _) => {
                val entry = readKeyAndValue(c)
                c.reader.pullAndAssertEquals(CloseDict)
                entry
              }
              case _ => throw new IncorrectTokenException("Expected OpenDict for each Map[_, _] entry")
            })

          }
          c.reader.pullAndAssertEquals(CloseArr)
          Map(entries: _*)
        }
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
      c.reader.pull match {
        case OpenArr(_) => {
          while (c.reader.peek != CloseArr) {
            b.append(reads.read(c))
          }
          c.reader.pullAndAssertEquals(CloseArr)
          b.toList
        }
        case _ => throw new IncorrectTokenException("Expected OpenArr at start of List[_]")
      }
    }
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
      c.reader.pull match {
        case OpenArr(_) => {
          while (c.reader.peek != CloseArr) {
            b.append(reads.read(c))
          }
          c.reader.pullAndAssertEquals(CloseArr)
          b.toSet
        }
        case _ => throw new IncorrectTokenException("Expected OpenArr at start of Set[_]")
      }
    }
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
        case OpenDict(_, _) => {
          val entries = ListBuffer[(String, V)]()
          while (c.reader.peek != CloseDict) {
            entries.append(c.reader.pull() match {
              case DictEntry(key) => {
                val value = reads.read(c)
                (key, value)
              }
              case _ => throw new IncorrectTokenException("Expected DictEntry at start of Map[String, _] entry")
            })
          }
          c.reader.pullAndAssertEquals(CloseDict)
          Map(entries: _*)
        }
        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }
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
        case NoneToken => {
          c.reader.pullAndAssertEquals(NoneToken)
          None
        }
        case _ => Some(reads.read(c))
      }
    }
  }
}

private object BoxFormatUtils {
  def write[T](box: Box[T], c: WriteContext, linkStrategy: LinkStrategy, writes: Writes[T]) {
    val id = box.id()
    linkStrategy match {

      case UseLinks => {
        if (c.writer.isBoxCached(id)) {
          c.writer.write(BoxToken(LinkRef(id)))
        } else {
          c.writer.write(BoxToken(LinkId(id)))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }
      }

      case NoLinks => {
        c.writer.write(BoxToken(LinkEmpty))
        writes.write(box.get()(c.txn), c)
      }

      case NoLinksOrDuplicates => {
        if (c.writer.isBoxCached(id)) {
          throw new BoxCacheException("Box id " + id + " was already cached, but boxLinkStrategy is NoLinksOrDuplicates")
        } else {
          c.writer.write(BoxToken(LinkEmpty))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }
      }
    }
  }

  def read[T](c: ReadContext, linkStrategy: LinkStrategy, reads: Reads[T]) = {
    c.reader.pull() match {
      case BoxToken(link) => {
        link match {
          case LinkEmpty => c.txn.create(reads.read(c))
          case LinkId(id) => {
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkId (" + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            val box = c.txn.create(reads.read(c))
            c.reader.putBox(id, box)
            box
          }
          case LinkRef(id) => {
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkRef( " + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            c.reader.getBox(id).asInstanceOf[Box[T]]
          }
        }
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
}

object BoxFormatsNoLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinks, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
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
}