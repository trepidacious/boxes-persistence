package org.rebeam.boxes.persistence.json

import org.rebeam.boxes.core.ShelfDefault
import org.rebeam.boxes.persistence._

object JsonDemo extends App {
  implicit val s = ShelfDefault()
  import PrimFormats._
  import ProductFormats._
  import BasicFormats._

  case class Person(firstName: String, lastName: String, age: Int, friend: Option[Person])

  implicit lazy val PersonFormat: Format[Person] = lazyFormat(productFormat4(Person)("firstName", "lastName", "age", "friend"))

  val alicia = Person("Alicia", "A", 42, None)
  val bob = Person("Bob", "B", 43, Some(alicia))

  println(s.read(implicit txn => JsonPrettyIO.toJsonString(bob)))
}
