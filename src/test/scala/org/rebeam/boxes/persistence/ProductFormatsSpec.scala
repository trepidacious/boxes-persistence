package org.rebeam.boxes.persistence

import boxes.transact.ShelfDefault
import org.rebeam.boxes.persistence.BasicFormats._
import org.rebeam.boxes.persistence.ProductFormats._
import org.rebeam.boxes.persistence.PrimFormats._
import org.scalatest._
import org.scalatest.prop.PropertyChecks

case class CaseClass6(b: Boolean, i: Int, l: Long, f: Float, d: Double, s: String)

case class Nested(i: Int, n: Option[Nested])

class ProductFormatsSpec extends WordSpec with PropertyChecks with ShouldMatchers {

  def duplicateCaseClass6(b: Boolean, i: Int, l: Long, f: Float, d: Double, s: String): Unit = {
    implicit val shelf = ShelfDefault()

    implicit val caseClassFormat = productFormat6(CaseClass6.apply)("b", "i", "l", "f", "d", "s")

    val c = CaseClass6(b, i, l, f, d, s)

    val writtenTokens = shelf.read(implicit txn => BufferIO.toTokens(c))

    val readC = shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass6](writtenTokens))

    readC shouldBe c
  }

  def duplicateCaseClass(s: String, i: Int): Unit ={
    implicit val shelf = ShelfDefault()

    implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")

    val c = CaseClass(s, i)

    val writtenTokens = shelf.read(implicit txn => BufferIO.toTokens(c))

    val readC = shelf.transact(implicit txn => BufferIO.fromTokens[CaseClass](writtenTokens))

    readC shouldBe c
  }

  "ProductFormats" should {

    "duplicate arbitrary case class of arity 6" in forAll{ (b: Boolean, i: Int, l: Long, f: Float, d: Double, s: String) => duplicateCaseClass6(b, i, l, f, d, s)}

    "duplicate arbitrary case class" in forAll { (s: String, i: Int) => duplicateCaseClass(s, i) }

    "duplicate nested case class" in {

      implicit val shelf = ShelfDefault()

      //TODO why do we need lazy val as well as lazyFormat? spray-json readme doesn't include it:
      // https://github.com/spray/spray-json
      //However similar examples in argonaut do use implicit lazy vals for formats:
      // https://github.com/argonaut-io/argonaut/issues/64
      //Note lazy val and format and Format[Nested] type annotation for nested case class, this is a pain
      //but otherwise we have a bit of a catch 22 ;)
      implicit lazy val nestedFormat: Format[Nested] = lazyFormat(productFormat2(Nested.apply)("i", "n"))

      val n = Nested(0, Some(Nested(1, Some(Nested(2, None)))))

      val writtenTokens = shelf.read(implicit txn => BufferIO.toTokens(n))

      val readN = shelf.transact(implicit txn => BufferIO.fromTokens[Nested](writtenTokens))

      readN shouldBe n
    }

    "read out of order fields" in {
      implicit val shelf = ShelfDefault()

      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")

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
      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")

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
      implicit val caseClassFormat = productFormat2(CaseClass.apply)("s", "i")

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
