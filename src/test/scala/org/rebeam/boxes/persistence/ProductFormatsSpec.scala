package org.rebeam.boxes.persistence

import boxes.transact.ShelfDefault
import org.rebeam.boxes.persistence.ProductFormats._
import org.rebeam.boxes.persistence.PrimFormats._
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ProductFormatsSpec extends WordSpec with PropertyChecks with ShouldMatchers {

  "ProductFormats" should {

    "read out of order fields" in  {
      implicit val shelf = ShelfDefault()

      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")()

      val c = CaseClass("p", 42)

      val writtenTokens = shelf.read(implicit txn => BufferIO.toTokens(c))

      val canonicalOrderTokens = List(
        OpenDict(NoName,LinkEmpty),
          DictEntry("s", LinkEmpty), StringToken("p"),
          DictEntry("i", LinkEmpty), IntToken(42),
        CloseDict
      )

      //Check that we write as expected - fields are in order used by CaseClass.apply and returned by c.productElement
      writtenTokens shouldBe canonicalOrderTokens

      //Check we can read the canonical order
      val readOrdered = shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](canonicalOrderTokens))
      readOrdered shouldBe c

      //Now reorder the two fields to test reading out of order (this can be caused for example by a round trip
      //through json tools that don't respect order)
      val outOfOrderTokens = List(
        OpenDict(NoName,LinkEmpty),
        DictEntry("i", LinkEmpty), IntToken(42),
        DictEntry("s", LinkEmpty), StringToken("p"),
        CloseDict
      )

      //Check that we can read out of order
      val readOutOfOrder = shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](outOfOrderTokens))
      readOutOfOrder shouldBe c
    }

    "fail with additional fields" in {
      implicit val shelf = ShelfDefault()
      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")()

      //This contains the correct fields, and also an extra one
      val additionalTokens = List(
        OpenDict(NoName,LinkEmpty),
        DictEntry("i", LinkEmpty), IntToken(42),
        DictEntry("j", LinkEmpty), IntToken(42),
        DictEntry("s", LinkEmpty), StringToken("p"),
        CloseDict
      )

      intercept[IncorrectTokenException] {
        shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](additionalTokens))
      }
    }

    "fail with missing fields" in {
      implicit val shelf = ShelfDefault()
      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")()

      //Here we are missing field i
      val missingFieldI = List(
        OpenDict(NoName,LinkEmpty),
        DictEntry("s", LinkEmpty), StringToken("p"),
        CloseDict
      )
      intercept[IncorrectTokenException] {
        shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](missingFieldI))
      }

      //Here we are missing field s
      val missingFieldS = List(
        OpenDict(NoName,LinkEmpty),
        DictEntry("i", LinkEmpty), IntToken(42),
        CloseDict
      )
      intercept[IncorrectTokenException] {
        shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](missingFieldS))
      }

    }

  }
}
