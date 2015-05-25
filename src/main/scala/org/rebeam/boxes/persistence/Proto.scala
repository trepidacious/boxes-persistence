package org.rebeam.boxes.persistence

import java.io.{StringWriter, ByteArrayInputStream, ByteArrayOutputStream}

import boxes.transact.{TxnR, Txn, Box, ShelfDefault}
import org.rebeam.boxes.persistence.json.JsonTokenWriter
import org.rebeam.boxes.persistence.protobuf.{ProtobufTokenReader, ProtobufTokenWriter}


case class Person(name: Box[String], age: Box[Int]) {
  def asString(implicit txn: TxnR) = "Person(" + name() + ", " + age() + ")"
}

object Person {
  def default(txn: Txn): Person = {
    implicit val t = txn
    Person(Box(""), Box(0))
  }
}

case class JsonStuff(name: Box[String], age: Box[Int], map: Box[Map[String, Person]], list: Box[List[Int]]) {
  def asString(implicit txn: TxnR) = "Person(" + name() + ", " + age() + ", " + map() + ", " + list() +")"
}

object JsonStuff {
  def default(txn: Txn): JsonStuff = {
    implicit val t = txn
    JsonStuff(Box(""), Box(0), Box(Map()), Box(List()))
  }
}


object Proto {

  def main(args: Array[String]): Unit = {
    implicit val shelf = ShelfDefault()

    import PrimFormats._
    import CollectionFormats._
    import ProductFormats._
    import NodeFormats._

    {

      val list = List(1, 2, 3, 4, 5)
      val intListTokens = shelf.transact(implicit txn => {
        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(list, c)
        println(w.tokens)
        w.tokens
      })

      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(intListTokens)
        val list = Reading.read[List[Int]](new ReadContext(r, txn))
        println(list)
      })

      println("Product format on list, token buffer, use links for nodes")
      implicit val personFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), boxLinkStrategy = EmptyLinks, nodeLinkStrategy = AllLinks)

      val bobTokens = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(List(bob, bob, bob), c)
        println(w.tokens)
        w.tokens
      })

      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(bobTokens)
        val list = Reading.read[List[Person]](new ReadContext(r, txn))
        println(list.map(_.asString))
      })
    }

    {
      println("Product format, token buffer")
      import BoxFormatsAllLinks._
      implicit val personFormat = productFormat2(Person.apply)("name", "age")(PresentationName("Person"))

      val bobTokens = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(bob, c)
        println(w.tokens)
        w.tokens
      })

      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(bobTokens)
        val bob2 = Reading.read[Person](new ReadContext(r, txn))
        println(bob2.asString)
      })
    }

    {
      println("Product format, protobuffer")
      import BoxFormatsAllLinks._
      implicit val personFormat = productFormat2(Person.apply)("name", "age")(PresentationName("Person"))

      val bobData = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val bos = new ByteArrayOutputStream()
        val w = ProtobufTokenWriter(bos)
        val c = WriteContext(w, txn)
        Writing.write(bob, c)
        w.close()
        bos.toByteArray
      })

      println("pb serialised to " + bobData.length + " bytes")

      shelf.transact(implicit txn => {
        val r = ProtobufTokenReader(new ByteArrayInputStream(bobData))
        val bob2 = Reading.read[Person](new ReadContext(r, txn))
        println("pb bob2: " + bob2.asString)
      })
    }

    {
      println("Node format use links, json")

      implicit val personNodeFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), EmptyLinks, AllLinks)

      val bobJson = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val sw = new StringWriter()
        val w = new JsonTokenWriter(sw)
        val c = WriteContext(w, txn)
        Writing.write(bob, c)
        w.close()
        sw.toString
      })

      println("json serialised to:\n" + bobJson)

//      shelf.transact(implicit txn => {
//        val r = ProtobufTokenReader(new ByteArrayInputStream(bobData))
//        val bob2 = Reading.read[Person](new ReadContext(r, txn))
//        println("pb bob2: " + bob2.asString)
//      })
    }

    {
      println("Node format no links, json")

      implicit val personNodeFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"), EmptyLinks, EmptyLinks)
      implicit val jsonStuffNodeFormat = nodeFormat4(JsonStuff.apply, JsonStuff.default)("name", "age", "map", "list")(PresentationName("JsonStuff"), EmptyLinks, EmptyLinks)

      val bobJson = shelf.transact(implicit txn => {
        val alice = Person(Box("Alice"), Box(32))
        val bob = Person(Box("Bob"), Box(34))
        val j = JsonStuff(Box("jsonStuff"), Box(24522), Box(Map("a" -> alice, "b" -> bob)), Box(List(1, 2, 3)))

        val sw = new StringWriter()
        val w = new JsonTokenWriter(sw)
        val c = WriteContext(w, txn)
        Writing.write(j, c)
        w.close()
        sw.toString
      })

      println("jsonStuff serialised to:\n" + bobJson)

      //      shelf.transact(implicit txn => {
      //        val r = ProtobufTokenReader(new ByteArrayInputStream(bobData))
      //        val bob2 = Reading.read[Person](new ReadContext(r, txn))
      //        println("pb bob2: " + bob2.asString)
      //      })
    }

    {
      println("Node format no links, json")

      implicit val personNodeFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"))

      val bobJson = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val sw = new StringWriter()
        val w = new JsonTokenWriter(sw)
        val c = WriteContext(w, txn)
        Writing.write(bob, c)
        w.close()
        sw.toString
      })

      println("json serialised to:\n" + bobJson)

      //      shelf.transact(implicit txn => {
      //        val r = ProtobufTokenReader(new ByteArrayInputStream(bobData))
      //        val bob2 = Reading.read[Person](new ReadContext(r, txn))
      //        println("pb bob2: " + bob2.asString)
      //      })
    }

    {
      println("Node format, token buffer")

      implicit val personNodeFormat = nodeFormat2(Person.apply, Person.default)("name", "age")(PresentationName("Person"))

      val bobTokens = shelf.transact(implicit txn => {
        val bob = Person(Box("Bob"), Box(34))

        val w = new BufferTokenWriter()
        val c = WriteContext(w, txn)
        Writing.write(bob, c)
        println(w.tokens)
        w.tokens
      })

      shelf.transact(implicit txn => {
        val r = new BufferTokenReader(bobTokens)
        val bob2 = Reading.read[Person](new ReadContext(r, txn))
        println(bob2.asString)
      })
    }
  }

  def main2(args: Array[String]): Unit = {

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
      import BoxFormatsAllLinks._

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
      import BoxFormatsEmptyLinks._

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
