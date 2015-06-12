package org.rebeam.boxes.persistence.gen

object NodeFormatsGen {

  /**
   * Generate a nodeFormat method of arbitrary arity
   *
   * Uses a template with the following values:
   * $fieldCount The number of fields in the product, e.g. "2"
   * $fieldTypes The parametric types of the product fields, e.g. "P1: Format, P2: Format, ..."
   * $constructorParameters The parameters to construct a new instance of the node, e.g. "Box[P1], Box[P2], ..."
   * $productNameParameters The parameters for the names of each field of the product, e.g. "name1: String, name2: String, ..."
   * $writeDictEntries The code to write all fields as dict entries, e.g. "writeDictEntry[P1](n, name1, 0, c, boxLinkStrategy)\nwriteDictEntry[P2](n, name2, 1, c, boxLinkStrategy)\n ..."
   * $useDictEntriesCases The cases to read all fields by calling useDictEntry, e.g. "case s if s == name1 => useDictEntry[P1](n, 0, c, link)\ncase s if s == name2 => useDictEntry[P2](n, 1, c, link)\n ..."
   *
   * @param n The arity of the product
   */
  private def genNodeFormat(n: Int) = {
    val indices = Range(1, n + 1)

    def format(f: (Int) => String, sep: String) = indices.map(f(_)).mkString(sep)

    val fieldCount =            "" + n

    val fieldTypes =            format(i => s"P$i: Format", ", ")

    val constructorParameters = format(i => s"Box[P$i]", ", ")

    val productNameParameters = format(i => s"name$i: String", ", ")

    val writeDictEntries =      format(i => s"writeDictEntry[P$i](n, name$i, ${i - 1}, c, boxLinkStrategy)", "\n      ")

    val useDictEntriesCases =   format(i => s"case s if s == name$i => useDictEntry[P$i](n, ${i - 1}, c, link)", "\n            ")

    s"""
      |  def nodeFormat$fieldCount[$fieldTypes, N <: Product](construct: ($constructorParameters) => N, default: (Txn) => N)
      |      ($productNameParameters)
      |      (name: TokenName = NoName, boxLinkStrategy: NoDuplicatesLinkStrategy = EmptyLinks, nodeLinkStrategy: LinkStrategy = EmptyLinks) : Format[N] = new Format[N] {
      |
      |    def writeEntriesAndClose(n: N, c: WriteContext): Unit = {
      |      $writeDictEntries
      |      c.writer.write(CloseDict)
      |    }
      |
      |    def readEntriesAndClose(c: ReadContext): N = {
      |      implicit val txn = c.txn
      |      val n = default(txn)
      |
      |      while (c.reader.peek != CloseDict) {
      |        c.reader.pull() match {
      |          case DictEntry(fieldName, link) => fieldName match {
      |            $useDictEntriesCases
      |            case x => throw new IncorrectTokenException("Unknown field name in Node dict " + x)
      |          }
      |          case x: Token => throw new IncorrectTokenException("Expected DictEntry in a Node Dict, got " + x)
      |        }
      |      }
      |
      |      c.reader.pullAndAssertEquals(CloseDict)
      |      n
      |    }
      |
      |    def write(n: N, c: WriteContext): Unit = writeNode(n, c, name, nodeLinkStrategy, writeEntriesAndClose)
      |    def read(c: ReadContext): N = readNode(c, readEntriesAndClose)
      |
      |  }
    """.stripMargin
  }

  def main(args: Array[String]) {
    for (i <- Range(1, 23)) println(genNodeFormat(i))
  }

}
