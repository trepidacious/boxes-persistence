package org.rebeam.boxes.persistence.gen

object ProductFormatsGen {

  /**
   * Generate a product Format method of arbitrary arity
   *
   * Uses a template with the following values:
   * $fieldCount The number of fields in the product, e.g. "2"
   * $fieldTypes The parametric types of the product fields, e.g. "P1: Format, P2: Format, ..."
   * $constructorParameters The parameters to construct a new instance of the node, e.g. "P1, P2, ..."
   * $productNameParameters The parameters for the names of each field of the product, e.g. "name1: String, name2: String, ..."
   * $writeDictEntries The code to write all fields as dict entries, e.g. "writeDictEntry[P1](p, name1, 0, c)\nwriteDictEntry[P2](p, name2, 1, c)\n ..."
   * $fieldVars The code to vreate field variables, e.g. "var p1: Option[P1] = None\nvar p2: Option[P2] = None\n ..."
   * $conditionalFieldReads The code to conditionally read fields baed on name, e.g. "if (n == name1) p1 = Some(implicitly[Format[P1]].read(c))\nelse if (n == name2) p2 = Some(implicitly[Format[P2]].read(c))\n ..."
   * $constructorArguments The arguments to product constructor, e.g. p1.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name1)),\np2.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name2))\n, ..."
   *
   * @param n The arity of the product
   */
  private def genProductFormat(n: Int) = {
    val indices = Range(1, n + 1)

    def format(f: (Int) => String, sep: String) = indices.map(f(_)).mkString(sep)

    val fieldCount =              "" + n

    val fieldTypes =              format(i => s"P$i: Format", ", ")

    val constructorParameters =   format(i => s"P$i", ", ")

    val productNameParameters =    format(i => s"name$i: String", ", ")

    val writeDictEntries =        format(i => s"writeDictEntry[P$i](p, name$i, ${i - 1}, c)", "\n      ")

    val fieldVars =               format(i => s"var p$i: Option[P$i] = None", "\n          ")

    val conditionalFieldReads =   format(i => s"if (n == name$i) p$i = Some(implicitly[Format[P$i]].read(c))", "\n                else ")

    val constructorArguments =    format(i => s"""p$i.getOrElse(throw new IncorrectTokenException("Product format has missing field " + name$i))""", ",\n            ")

    s"""
      |  def productFormat$fieldCount[$fieldTypes, P <: Product](construct: ($constructorParameters) => P)
      |  ($productNameParameters)
      |  (name: TokenName = NoName) : Format[P] = new Format[P] {
      |
      |    def write(p: P, c: WriteContext): Unit = {
      |      c.writer.write(OpenDict(name))
      |      $writeDictEntries
      |      c.writer.write(CloseDict)
      |    }
      |
      |    def read(c: ReadContext): P = {
      |      c.reader.pull() match {
      |        case OpenDict(_, _) =>
      |
      |          $fieldVars
      |
      |          while (c.reader.peek != CloseDict) {
      |            c.reader.pull() match {
      |              case DictEntry(n, LinkEmpty) =>
      |                $conditionalFieldReads
      |                else throw new IncorrectTokenException("Product format has unrecognised name " + n)
      |
      |              case t => throw new IncorrectTokenException("Product format has unexpected token " + t)
      |            }
      |          }
      |
      |          val p = construct(
      |            $constructorArguments
      |          )
      |          c.reader.pullAndAssertEquals(CloseDict)
      |          p
      |
      |        case _ => throw new IncorrectTokenException("Expected OpenDict at start of Map[String, _]")
      |      }
      |    }
      |
      |  }
    """.stripMargin
  }

  def main(args: Array[String]) {
    for (i <- Range(1, 23)) println(genProductFormat(i))
  }

}
