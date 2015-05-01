package org.rebeam.boxes.persistence

import scala.collection.mutable.ListBuffer

object PrimFormats {
  implicit val booleanFormat = new Format[Boolean] {
    def write(s: Boolean, c: WriteContext) = c.writer.write(BooleanToken(s))
    def read(context: ReadContext) = context.reader.pullBoolean()
  }
  implicit val intFormat = new Format[Int] {
    def write(s: Int, c: WriteContext) = c.writer.write(IntToken(s))
    def read(context: ReadContext) = context.reader.pullInt()
  }
  implicit val longFormat = new Format[Long] {
    def write(s: Long, c: WriteContext) = c.writer.write(LongToken(s))
    def read(context: ReadContext) = context.reader.pullLong()
  }
  implicit val floatFormat = new Format[Float] {
    def write(s: Float, c: WriteContext) = c.writer.write(FloatToken(s))
    def read(context: ReadContext) = context.reader.pullFloat()
  }
  implicit val doubleFormat = new Format[Double] {
    def write(s: Double, c: WriteContext) = c.writer.write(DoubleToken(s))
    def read(context: ReadContext) = context.reader.pullDouble()
  }
  implicit val stringFormat = new Format[String] {
    def write(s: String, c: WriteContext) = c.writer.write(StringToken(s))
    def read(context: ReadContext) = context.reader.pullString()
  }
}

object CollectionFormats{
  implicit def writesList[T](implicit writes: Writes[T]) = new Writes[List[T]] {
    def write(list: List[T], c: WriteContext) {
      c.writer.write(OpenArr)
      list.foreach(t => writes.write(t, c))
      c.writer.write(CloseArr)
    }
  }
  implicit def readsList[T](implicit reads: Reads[T]) = new Reads[List[T]] {
    def read(c: ReadContext) = {
      val b = ListBuffer[T]()
      c.reader.pullAndAssert(OpenArr)
      while (c.reader.peek != CloseArr) {
        b.append(reads.read(c))
      }
      c.reader.pullAndAssert(CloseArr)
      b.toList
    }
  }
}