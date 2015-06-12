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
   */
  def lazyFormat[T](format: => Format[T]) = new Format[T] {
    lazy val delegate = format
    def write(t: T, c: WriteContext) = delegate.write(t, c)
    def read(c: ReadContext) = delegate.read(c)
  }

}