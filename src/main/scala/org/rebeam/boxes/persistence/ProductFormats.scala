package org.rebeam.boxes.persistence

//TODO should this have support for dict links?
object ProductFormats {

  private def writeDictEntry[T :Format](p: Product, name: String, index: Int, c: WriteContext): Unit = {
    c.writer.write(DictEntry(name))
    implicitly[Format[T]].write(p.productElement(index).asInstanceOf[T], c)
  }

  def productFormat2[P1: Format, P2: Format, P <: Product](construct: (P1, P2) => P)(name1: String, name2: String)(name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) => {
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)
              }
              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val n = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          n

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }
}