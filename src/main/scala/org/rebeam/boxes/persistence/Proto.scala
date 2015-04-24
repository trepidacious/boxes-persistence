import boxes.transact.ShelfDefault
import org.rebeam.boxes.persistence._

object Proto {
  def main(args: Array[String]): Unit = {

    implicit val shelf = ShelfDefault()

    import WritesString._
    import WritesInt._
    import WritesList._

    shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      Writing.write(List("A", "B", "C"), c)
      println(w.tokens)
    })

    shelf.read(implicit txn => {
      val w = new BufferTokenWriter()
      val c = WriteContext(w, txn)
      println(Writing.write(List(1, 2, 3), c))
      println(w.tokens)
    })

  }
}
