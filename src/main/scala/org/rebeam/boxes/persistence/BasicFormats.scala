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
}