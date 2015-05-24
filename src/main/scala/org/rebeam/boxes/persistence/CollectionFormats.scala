package org.rebeam.boxes.persistence

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

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
              case DictEntry(key, LinkEmpty) =>
                val value = reads.read(c)
                (key, value)

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