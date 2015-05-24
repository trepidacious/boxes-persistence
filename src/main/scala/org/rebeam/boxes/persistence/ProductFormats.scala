package org.rebeam.boxes.persistence

import scala.reflect.ClassTag

//TODO should this have support for dict links?
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