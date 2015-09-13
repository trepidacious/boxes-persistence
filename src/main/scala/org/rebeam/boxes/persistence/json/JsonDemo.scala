package org.rebeam.boxes.persistence.json

import org.rebeam.boxes.core.ShelfDefault
import org.rebeam.boxes.persistence._

object JsonDemo extends App {
  implicit val s = ShelfDefault()
  import PrimFormats._
  import ProductFormats._
  import BasicFormats._
  import CollectionFormats._

  case class Person(firstName: String, lastName: String, age: Int, friend: Option[Person], favouriteColors: List[String])
  implicit lazy val PersonFormat: Format[Person] = lazyFormat(productFormat5(Person)("firstName", "lastName", "age", "friend", "favouriteColors"))

  val alicia = Person("Alicia", "A", 42, None, List("red", "blue"))
  val bob = Person("Bob", "B", 43, Some(alicia), List("grey"))

  println(s.read(implicit txn => JsonPrettyIO.toJsonString(bob)))
  println(s.read(implicit txn => JsonPrettyIO.toJsonString(List(1, 2, 3, 4))))


  case object Thing

  implicit lazy val ThingFormat = productFormat0(Thing)()

  println(s.read(implicit txn => JsonPrettyIO.toJsonString(Thing)))
}
