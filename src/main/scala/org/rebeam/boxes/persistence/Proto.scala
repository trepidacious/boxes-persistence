import boxes.transact.{Box, ShelfDefault}
import org.rebeam.boxes.persistence._

object Proto {
  def main(args: Array[String]): Unit = {

    implicit val shelf = ShelfDefault()

    import PrimFormats._
    import CollectionFormats._
    import BasicFormats._

    shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(List("A", "B", "C"), c)
      println(w.tokens)
    })

    val listIntTokens = shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(List(1, 2, 3), c)
      println(w.tokens)
      w.tokens
    })

    shelf.transact(implicit txn => {
      val r = new BufferTokenReader(listIntTokens)
      val list = Reading.read[List[Int]](new ReadContext(r, txn))
      println(list)
    })

    val optionTokens = shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(Set(Option("SomeValue"), None), c)
      println(w.tokens)
      w.tokens
    })

    shelf.transact(implicit txn => {
      val r = new BufferTokenReader(optionTokens)
      val list = Reading.read[Set[Option[String]]](new ReadContext(r, txn))
      println(list)
    })

    val stringMapTokens = shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(Map("a" -> 1, "b" -> 2), c)
      println(w.tokens)
      w.tokens
    })

    shelf.transact(implicit txn => {
      val r = new BufferTokenReader(stringMapTokens)
      val list = Reading.read[Map[String, Int]](new ReadContext(r, txn))
      println(list)
    })

    val mapTokens = shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(Map(1 -> 2, 2 -> 4), c)
      println(w.tokens)
      w.tokens
    })

    shelf.transact(implicit txn => {
      val r = new BufferTokenReader(mapTokens)
      val list = Reading.read[Map[Int, Int]](new ReadContext(r, txn))
      println(list)
    })

    val bobBox = shelf.create("bob")

    {
      import BoxFormatsUseLinks._

      val boxTokens = shelf.read(implicit txn => {

        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(List(bobBox, bobBox), c)
        println(w.tokens)
        w.tokens
      })
      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(boxTokens)
        val boxList = Reading.read[List[Box[String]]](new ReadContext(r, txn))
        println(boxList(0) + ", value " + boxList(0)() + ", " + boxList(1) + ", value " + boxList(1)())
      })
    }

    {
      import BoxFormatsNoLinks._

      val boxTokens2 = shelf.read(implicit txn => {
        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(List(bobBox, bobBox), c)
        println(w.tokens)
        w.tokens
      })
      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(boxTokens2)
        val boxList = Reading.read[List[Box[String]]](new ReadContext(r, txn))
        println(boxList(0) + ", value " + boxList(0)() + ", " + boxList(1) + ", value " + boxList(1)())
      })
    }
  }
}
