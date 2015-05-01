import boxes.transact.ShelfDefault
import org.rebeam.boxes.persistence._

object Proto {
  def main(args: Array[String]): Unit = {

    implicit val shelf = ShelfDefault()

    import PrimFormats._
    import CollectionFormats._

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

  }
}
