package org.rebeam.boxes.persistence

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import boxes.transact.{ShelfDefault, Txn, TxnR, Box}
import org.rebeam.boxes.persistence.NodeFormats._
import org.rebeam.boxes.persistence.PrimFormats._
import org.rebeam.boxes.persistence.CollectionFormats._
import org.rebeam.boxes.persistence.protobuf.ProtobufIO
import org.scalatest._
import org.scalatest.prop.{TableDrivenPropertyChecks, PropertyChecks}

case class Person(name: Box[String], age: Box[Int]) {
  def asString(implicit txn: TxnR) = "Person(" + name() + ", " + age() + ")"
}

object Person {
  def default(txn: Txn): Person = {
    implicit val t = txn
    Person(Box(""), Box(0))
  }
  def apply(name: String, age: Int)(implicit txn: Txn): Person = {
    Person(Box(name), Box(age))
  }
}

class ProtobufSpec extends WordSpec with PropertyChecks with ShouldMatchers {

  def duplicateList[T](list: List[T])(implicit format: Format[List[T]]) = {
    implicit val shelf = ShelfDefault()
    val os = new ByteArrayOutputStream()
    ProtobufIO.writeNow(list, os)
    val list2 = ProtobufIO.readNow[List[T]](new ByteArrayInputStream(os.toByteArray))
    list shouldBe list2
  }

  def duplicatePerson(name: String, age: Int) = {
    implicit val shelf = ShelfDefault()
    implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy = NoLinksOrDuplicates, nodeLinkStrategy = UseLinks)

    val bob = shelf.transact(implicit txn => Person(name, age))

    val os = new ByteArrayOutputStream()
    ProtobufIO.writeNow(bob, os)

    val bob2 = ProtobufIO.readNow[Person](new ByteArrayInputStream(os.toByteArray))

    val os2 = new ByteArrayOutputStream()
    ProtobufIO.writeNow(bob2, os2)

    os.toByteArray shouldBe os2.toByteArray

    shelf.read(implicit txn => {
      bob.name() shouldBe bob2.name()
      bob.age() shouldBe bob2.age()
    })
  }

  "ProtobufIO" should {

    "duplicate Person" in duplicatePerson("bob", 34)

    "duplicate List[Person]" in {
      implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy = NoLinksOrDuplicates, nodeLinkStrategy = UseLinks)
      implicit val shelf = ShelfDefault()
      val list = shelf.transact(implicit txn => List(Person("a", 1), Person("b", 2), Person("c", 3), Person("d", 4), Person("e", 5)))
      val os = new ByteArrayOutputStream()
      ProtobufIO.writeNow(list, os)
      val list2 = ProtobufIO.readNow[List[Person]](new ByteArrayInputStream(os.toByteArray))

      shelf.read(implicit txn => {
        list.size shouldBe list2.size
        list.zip(list2).foreach{case (a,b) => {
          a.name() shouldBe b.name()
          a.age() shouldBe b.age()
        }}
      })
    }

    "duplicate arbitrary List[Int]" in forAll{ (list: List[Int]) => duplicateList(list)}
    "duplicate arbitrary List[Double]" in forAll{ (list: List[Double]) => duplicateList(list)}
    "duplicate arbitrary List[Long]" in forAll{ (list: List[Long]) => duplicateList(list)}
    "duplicate arbitrary List[Float]" in forAll{ (list: List[Float]) => duplicateList(list)}
    "duplicate arbitrary List[Boolean]" in forAll{ (list: List[Boolean]) => duplicateList(list)}
    "duplicate arbitrary List[String]" in forAll{ (list: List[String]) => duplicateList(list)}

    "duplicate arbitrary Person" in forAll{ (name: String, age: Int) => duplicatePerson(name, age)}

  }
}
