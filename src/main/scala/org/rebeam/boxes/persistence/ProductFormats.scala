package org.rebeam.boxes.persistence

//TODO should this have support for dict links?
object ProductFormats {

  private def writeDictEntry[T :Format](p: Product, name: String, index: Int, c: WriteContext): Unit = {
    c.writer.write(DictEntry(name))
    implicitly[Format[T]].write(p.productElement(index).asInstanceOf[T], c)
  }






  def productFormat1[P1: Format, P <: Product](construct: (P1) => P)
                                              (name1: String)
                                              (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat2[P1: Format, P2: Format, P <: Product](construct: (P1, P2) => P)
                                                          (name1: String, name2: String)
                                                          (name: TokenName = NoName) : Format[P] = new Format[P] {

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
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat3[P1: Format, P2: Format, P3: Format, P <: Product](construct: (P1, P2, P3) => P)
                                                                      (name1: String, name2: String, name3: String)
                                                                      (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat4[P1: Format, P2: Format, P3: Format, P4: Format, P <: Product](construct: (P1, P2, P3, P4) => P)
                                                                                  (name1: String, name2: String, name3: String, name4: String)
                                                                                  (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat5[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P <: Product](construct: (P1, P2, P3, P4, P5) => P)
                                                                                              (name1: String, name2: String, name3: String, name4: String, name5: String)
                                                                                              (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat6[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6) => P)
                                                                                                          (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String)
                                                                                                          (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat7[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7) => P)
                                                                                                                      (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String)
                                                                                                                      (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat8[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8) => P)
                                                                                                                                  (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String)
                                                                                                                                  (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat9[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9) => P)
                                                                                                                                              (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String)
                                                                                                                                              (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat10[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10) => P)
                                                                                                                                                            (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String)
                                                                                                                                                            (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat11[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11) => P)
                                                                                                                                                                         (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String)
                                                                                                                                                                         (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat12[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12) => P)
                                                                                                                                                                                      (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String)
                                                                                                                                                                                      (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat13[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13) => P)
                                                                                                                                                                                                   (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String)
                                                                                                                                                                                                   (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat14[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14) => P)
                                                                                                                                                                                                                (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String)
                                                                                                                                                                                                                (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat15[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15) => P)
                                                                                                                                                                                                                             (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String)
                                                                                                                                                                                                                             (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat16[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16) => P)
                                                                                                                                                                                                                                          (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String)
                                                                                                                                                                                                                                          (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat17[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17) => P)
                                                                                                                                                                                                                                                       (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String)
                                                                                                                                                                                                                                                       (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat18[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18) => P)
                                                                                                                                                                                                                                                                    (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String)
                                                                                                                                                                                                                                                                    (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      writeDictEntry[P18](p, name18, 17, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None
          var p18: Option[P18] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else if (n == name18) p18 = Some(implicitly[Format[P18]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17)),
            p18.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name18))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat19[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19) => P)
                                                                                                                                                                                                                                                                                 (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String)
                                                                                                                                                                                                                                                                                 (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      writeDictEntry[P18](p, name18, 17, c)
      writeDictEntry[P19](p, name19, 18, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None
          var p18: Option[P18] = None
          var p19: Option[P19] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else if (n == name18) p18 = Some(implicitly[Format[P18]].read(c))
                else if (n == name19) p19 = Some(implicitly[Format[P19]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17)),
            p18.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name18)),
            p19.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name19))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat20[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20) => P)
                                                                                                                                                                                                                                                                                              (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String)
                                                                                                                                                                                                                                                                                              (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      writeDictEntry[P18](p, name18, 17, c)
      writeDictEntry[P19](p, name19, 18, c)
      writeDictEntry[P20](p, name20, 19, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None
          var p18: Option[P18] = None
          var p19: Option[P19] = None
          var p20: Option[P20] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else if (n == name18) p18 = Some(implicitly[Format[P18]].read(c))
                else if (n == name19) p19 = Some(implicitly[Format[P19]].read(c))
                else if (n == name20) p20 = Some(implicitly[Format[P20]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17)),
            p18.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name18)),
            p19.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name19)),
            p20.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name20))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat21[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, P21: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21) => P)
                                                                                                                                                                                                                                                                                                           (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String, name21: String)
                                                                                                                                                                                                                                                                                                           (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      writeDictEntry[P18](p, name18, 17, c)
      writeDictEntry[P19](p, name19, 18, c)
      writeDictEntry[P20](p, name20, 19, c)
      writeDictEntry[P21](p, name21, 20, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None
          var p18: Option[P18] = None
          var p19: Option[P19] = None
          var p20: Option[P20] = None
          var p21: Option[P21] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else if (n == name18) p18 = Some(implicitly[Format[P18]].read(c))
                else if (n == name19) p19 = Some(implicitly[Format[P19]].read(c))
                else if (n == name20) p20 = Some(implicitly[Format[P20]].read(c))
                else if (n == name21) p21 = Some(implicitly[Format[P21]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17)),
            p18.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name18)),
            p19.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name19)),
            p20.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name20)),
            p21.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name21))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }


  def productFormat22[P1: Format, P2: Format, P3: Format, P4: Format, P5: Format, P6: Format, P7: Format, P8: Format, P9: Format, P10: Format, P11: Format, P12: Format, P13: Format, P14: Format, P15: Format, P16: Format, P17: Format, P18: Format, P19: Format, P20: Format, P21: Format, P22: Format, P <: Product](construct: (P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14, P15, P16, P17, P18, P19, P20, P21, P22) => P)
                                                                                                                                                                                                                                                                                                                        (name1: String, name2: String, name3: String, name4: String, name5: String, name6: String, name7: String, name8: String, name9: String, name10: String, name11: String, name12: String, name13: String, name14: String, name15: String, name16: String, name17: String, name18: String, name19: String, name20: String, name21: String, name22: String)
                                                                                                                                                                                                                                                                                                                        (name: TokenName = NoName) : Format[P] = new Format[P] {

    def write(p: P, c: WriteContext): Unit = {
      c.writer.write(OpenDict(name))
      writeDictEntry[P1](p, name1, 0, c)
      writeDictEntry[P2](p, name2, 1, c)
      writeDictEntry[P3](p, name3, 2, c)
      writeDictEntry[P4](p, name4, 3, c)
      writeDictEntry[P5](p, name5, 4, c)
      writeDictEntry[P6](p, name6, 5, c)
      writeDictEntry[P7](p, name7, 6, c)
      writeDictEntry[P8](p, name8, 7, c)
      writeDictEntry[P9](p, name9, 8, c)
      writeDictEntry[P10](p, name10, 9, c)
      writeDictEntry[P11](p, name11, 10, c)
      writeDictEntry[P12](p, name12, 11, c)
      writeDictEntry[P13](p, name13, 12, c)
      writeDictEntry[P14](p, name14, 13, c)
      writeDictEntry[P15](p, name15, 14, c)
      writeDictEntry[P16](p, name16, 15, c)
      writeDictEntry[P17](p, name17, 16, c)
      writeDictEntry[P18](p, name18, 17, c)
      writeDictEntry[P19](p, name19, 18, c)
      writeDictEntry[P20](p, name20, 19, c)
      writeDictEntry[P21](p, name21, 20, c)
      writeDictEntry[P22](p, name22, 21, c)
      c.writer.write(CloseDict)
    }

    def read(c: ReadContext): P = {
      c.reader.pull() match {
        case OpenDict(_, _) =>

          var p1: Option[P1] = None
          var p2: Option[P2] = None
          var p3: Option[P3] = None
          var p4: Option[P4] = None
          var p5: Option[P5] = None
          var p6: Option[P6] = None
          var p7: Option[P7] = None
          var p8: Option[P8] = None
          var p9: Option[P9] = None
          var p10: Option[P10] = None
          var p11: Option[P11] = None
          var p12: Option[P12] = None
          var p13: Option[P13] = None
          var p14: Option[P14] = None
          var p15: Option[P15] = None
          var p16: Option[P16] = None
          var p17: Option[P17] = None
          var p18: Option[P18] = None
          var p19: Option[P19] = None
          var p20: Option[P20] = None
          var p21: Option[P21] = None
          var p22: Option[P22] = None

          while (c.reader.peek != CloseDict) {
            c.reader.pull() match {
              case DictEntry(n, LinkEmpty) =>
                if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))
                else if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))
                else if (n == name3) p3 = Some(implicitly[Format[P3]].read(c))
                else if (n == name4) p4 = Some(implicitly[Format[P4]].read(c))
                else if (n == name5) p5 = Some(implicitly[Format[P5]].read(c))
                else if (n == name6) p6 = Some(implicitly[Format[P6]].read(c))
                else if (n == name7) p7 = Some(implicitly[Format[P7]].read(c))
                else if (n == name8) p8 = Some(implicitly[Format[P8]].read(c))
                else if (n == name9) p9 = Some(implicitly[Format[P9]].read(c))
                else if (n == name10) p10 = Some(implicitly[Format[P10]].read(c))
                else if (n == name11) p11 = Some(implicitly[Format[P11]].read(c))
                else if (n == name12) p12 = Some(implicitly[Format[P12]].read(c))
                else if (n == name13) p13 = Some(implicitly[Format[P13]].read(c))
                else if (n == name14) p14 = Some(implicitly[Format[P14]].read(c))
                else if (n == name15) p15 = Some(implicitly[Format[P15]].read(c))
                else if (n == name16) p16 = Some(implicitly[Format[P16]].read(c))
                else if (n == name17) p17 = Some(implicitly[Format[P17]].read(c))
                else if (n == name18) p18 = Some(implicitly[Format[P18]].read(c))
                else if (n == name19) p19 = Some(implicitly[Format[P19]].read(c))
                else if (n == name20) p20 = Some(implicitly[Format[P20]].read(c))
                else if (n == name21) p21 = Some(implicitly[Format[P21]].read(c))
                else if (n == name22) p22 = Some(implicitly[Format[P22]].read(c))
                else throw new IncorrectTokenException("Product format has unrecognised name " + n)

              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
            }
          }

          val p = construct(
            p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),
            p2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2)),
            p3.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name3)),
            p4.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name4)),
            p5.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name5)),
            p6.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name6)),
            p7.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name7)),
            p8.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name8)),
            p9.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name9)),
            p10.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name10)),
            p11.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name11)),
            p12.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name12)),
            p13.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name13)),
            p14.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name14)),
            p15.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name15)),
            p16.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name16)),
            p17.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name17)),
            p18.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name18)),
            p19.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name19)),
            p20.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name20)),
            p21.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name21)),
            p22.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name22))
          )
          c.reader.pullAndAssertEquals(CloseDict)
          p

        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      }
    }

  }

}