package org.rebeam.boxes.persistence

import org.rebeam.boxes.core.{Txn, TxnR, Box, ShelfDefault}
import org.scalatest.Matchers

object PersistenceSpecUtils extends Matchers {
  def duplicate[T](t: T, format: Format[T]): Unit = {
    implicit val f = format
    implicit val shelf = ShelfDefault()
    val writtenTokens = shelf.read(implicit txn => BufferIO.toTokens(t))
    val read = shelf.transact(implicit txn => BufferIO.fromTokens[T](writtenTokens))
    read shouldBe t
  }
}

case class Person(name: Box[String], age: Box[Int]) {
  def asString(implicit txn: TxnR) = "Person(" + name() + ", " + age() + ")"
}

object Person {
  def default(implicit txn: Txn): Person = {
    Person(Box(""), Box(0))
  }
}

case class CaseClass(s: String, i: Int)
