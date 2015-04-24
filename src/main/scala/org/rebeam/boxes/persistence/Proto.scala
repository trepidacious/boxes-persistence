import org.rebeam.boxes.persistence._

object Proto {
  def main(args: Array[String]): Unit = {
    import WritesStringToString._
    import WritesIntToString._
    import WritesListToString._

    println(Writing.write(List("A", "B", "C"), ""))
    println(Writing.write(List(1, 2, 3), ""))
//    println(Writing.write(List(1.0, 2, 3), ""))

    import WritesStringTokens._
    import WritesIntTokens._
    import WritesListTokens._

    val c = new TokenTargetBuffer()
    Writing.write("A", c)
//    Writing.write(List("A", "B", "C"), c)
    println(c)
  }
}
