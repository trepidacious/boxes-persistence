package org.rebeam.boxes.persistence

import scala.language.implicitConversions

object BasicFormats {
  implicit def writesOption[T](implicit writes: Writes[T]): Writes[Option[T]] = new Writes[Option[T]] {
    def write(option: Option[T], c: WriteContext) {
      option match {
        case Some(v) => writes.write(v, c)
        case None => c.writer.write(NoneToken)
      }
    }
  }
  implicit def readsOption[T](implicit reads: Reads[T]): Reads[Option[T]] = new Reads[Option[T]] {
    def read(c: ReadContext) = {
      c.reader.peek match {
        case NoneToken =>
          c.reader.pullAndAssertEquals(NoneToken)
          None

        case _ => Some(reads.read(c))
      }
    }
  }

  implicit def optionFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Option[T]] = new Format[Option[T]] {
    override def read(context: ReadContext): Option[T] = readsOption[T].read(context)
    override def write(obj: Option[T], context: WriteContext): Unit = writesOption[T].write(obj, context)
  }

  /**
   * Lazy format for nested/recursive case classes/nodes
   * To use, wrap the normal Format for the recursive data, add a Format[T] annotation, and assign to a lazy val, e.g.
   *
   * case class Nested(i: Int, n: Option[Nested])
   * implicit lazy val nestedFormat: Format[Nested] = lazyFormat(productFormat2(Nested.apply)("i", "n"))
   *
   * This breaks the cycle that would otherwise result in a compile time error or runtime stack overflow.
   *
   * @param format A non-lazy format to which to lazily delegate
   */
  def lazyFormat[T](format: => Format[T]) = new Format[T] {
    lazy val delegate = format
    def write(t: T, c: WriteContext) = delegate.write(t, c)
    def read(c: ReadContext) = delegate.read(c)
  }

}