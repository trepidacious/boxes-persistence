package org.rebeam.boxes.persistence

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import boxes.transact.{ShelfDefault, Txn, TxnR, Box}
import org.rebeam.boxes.persistence.NodeFormats._
import org.rebeam.boxes.persistence.PrimFormats._
import org.rebeam.boxes.persistence.CollectionFormats._
import org.rebeam.boxes.persistence.protobuf.ProtobufIO
import org.scalatest._
import org.scalatest.prop.PropertyChecks

case class Person(name: Box[String], age: Box[Int]) {
  def asString(implicit txn: TxnR) = "Person(" + name() + ", " + age() + ")"
}

object Person {
  def default(txn: Txn): Person = {
    implicit val t = txn
    Person(Box(""), Box(0))
  }
}

class ProtobufSpec extends WordSpec with PropertyChecks with ShouldMatchers {

  def makePerson(name: String, age: Int)(implicit txn: Txn): Person = {
    Person(Box(name), Box(age))
  }

  def duplicateList[T](list: List[T])(implicit format: Format[List[T]]) = {
    implicit val shelf = ShelfDefault()
    val os = new ByteArrayOutputStream()
    ProtobufIO.writeNow(list, os)
    val list2 = ProtobufIO.readNow[List[T]](new ByteArrayInputStream(os.toByteArray))
    list shouldBe list2
  }

  def duplicatePerson(name: String, age: Int) = {
    implicit val shelf = ShelfDefault()
    implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy = EmptyLinks, nodeLinkStrategy = AllLinks)

    val bob = shelf.transact(implicit txn => makePerson(name, age))

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

  def duplicateIdenticalPersonList(boxLinkStrategy: NoDuplicatesLinkStrategy, nodeLinkStrategy: LinkStrategy) = {
    implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy, nodeLinkStrategy)

    implicit val shelf = ShelfDefault()

    //Create two persons with equal contents, but not the same person (not identical),
    //and a list that contains each one twicw
    val list = shelf.transact(implicit txn => {
      val a = makePerson("a", 1)
      val b = makePerson("a", 1)
      List(a, a, b, b)
    })

    //Check that each a is equal to the other a, and that a is not equal to b
    list(0) should be (list(1))
    list(2) should be (list(3))
    list(0) should not be list(2)
    list(1) should not be list(3)

    val os = new ByteArrayOutputStream()
    ProtobufIO.writeNow(list, os)

    //Check that after writing and reading, we again have identical (equal) Persons in the first two
    //positions and last two positions, but they are not equal to each other, or the corresponding
    //Persons in the original array
    val list2 = ProtobufIO.readNow[List[Person]](new ByteArrayInputStream(os.toByteArray))
    list2(0) should be (list2(1))
    list2(2) should be (list2(3))
    list2(0) should not be list2(2)
    list2(1) should not be list2(3)
    list(0) should not be list2(0)
    list(1) should not be list2(1)
    list(2) should not be list2(2)
    list(3) should not be list2(3)
  }

  "ProtobufIO" should {

    "duplicate Person" in duplicatePerson("bob", 34)

    "duplicate List[Person]" in {
      implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy = EmptyLinks, nodeLinkStrategy = AllLinks)
      implicit val shelf = ShelfDefault()
      val list = shelf.transact(implicit txn => List(makePerson("a", 1), makePerson("b", 2), makePerson("c", 3), makePerson("d", 4), makePerson("e", 5)))
      val os = new ByteArrayOutputStream()
      ProtobufIO.writeNow(list, os)
      val list2 = ProtobufIO.readNow[List[Person]](new ByteArrayInputStream(os.toByteArray))

      shelf.read(implicit txn => {
        list.size shouldBe list2.size
        list.zip(list2).foreach{case (first, second) =>
          first.name() shouldBe second.name()
          first.age() shouldBe second.age()
        }
      })
    }

    "duplicate arbitrary lists " in {
      forAll{ (list: List[Int]) => duplicateList(list)};      info("of Int")
      forAll{ (list: List[Double]) => duplicateList(list)};   info("of Double")
      forAll{ (list: List[Long]) => duplicateList(list)};     info("of Long")
      forAll{ (list: List[Float]) => duplicateList(list)};    info("of Float")
      forAll{ (list: List[Boolean]) => duplicateList(list)};  info("of Boolean")
      forAll{ (list: List[String]) => duplicateList(list)};   info("of String")
    }

    "duplicate arbitrary Person" in forAll{ (name: String, age: Int) => duplicatePerson(name, age)}

    "preserve identicality of Persons with AllLinks for nodes" in {
      duplicateIdenticalPersonList(EmptyLinks, AllLinks)
      info ("Boxes with empty links")

      duplicateIdenticalPersonList(IdLinks, AllLinks)
      info ("Boxes with id links")
    }

    "fail with NodeCacheException when writing duplicate persons without all links" in {

      intercept[NodeCacheException] {
        duplicateIdenticalPersonList(boxLinkStrategy = EmptyLinks, nodeLinkStrategy = EmptyLinks)
      }
      info ("Boxes and nodes with empty links")

      intercept[NodeCacheException] {
        duplicateIdenticalPersonList(boxLinkStrategy = IdLinks, nodeLinkStrategy = EmptyLinks)
      }
      info ("Boxes with id links, nodes with empty links")

      intercept[NodeCacheException] {
        duplicateIdenticalPersonList(boxLinkStrategy = EmptyLinks, nodeLinkStrategy = IdLinks)
      }
      info ("Boxes with empty links, nodes with id links")

      intercept[NodeCacheException] {
        duplicateIdenticalPersonList(boxLinkStrategy = IdLinks, nodeLinkStrategy = IdLinks)
      }
      info ("Boxes and nodes with id links")
    }


  }
}
