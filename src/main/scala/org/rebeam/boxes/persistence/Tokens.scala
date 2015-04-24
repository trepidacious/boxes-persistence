package org.rebeam.boxes.persistence

sealed trait Token

case object OpenObj extends Token
case object CloseObj extends Token

case class Box(id: Int) extends Token

sealed trait Prim[P] extends Token {
  def p: P
}

case class BooleanToken(p: Boolean) extends Prim[Boolean]
case class IntToken(p: Int) extends Prim[Int]
case class LongToken(p: Long) extends Prim[Long]
case class FloatToken(p: Float) extends Prim[Float]
case class DoubleToken(p: Double) extends Prim[Double]
case class StringToken(p: String) extends Prim[String]

case class OpenField(name: String) extends Token

case object OpenArr extends Token
case object CloseArr extends Token

case object End extends Token


//Result of checking for an object in the cache
sealed abstract class CacheResult
//Object is already cached, use a ref as given
case class Cached(ref:Int) extends CacheResult
//Object was not cached, use an id as given
case class New(id:Int) extends CacheResult

import boxes.transact.Txn
import boxes.transact.TxnR

import annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

trait TokenWriter {
  def write(t: Token)
}

case class WriteContext(target: TokenWriter, txn: TxnR)

trait TokenReader {
  def peek: Token
  def pull(): Token

  def pullAndAssert(t:Token) {
    val p = pull()
    if (p != t) throw new RuntimeException("Expected " + t + ", got " + pull)
  }
}

case class ReadContext(source: TokenReader, txn: Txn)

class BufferTokenWriter extends TokenWriter {
  private val buffer = ListBuffer[Token]()
  override def write(t: Token) = {
    buffer.append(t)
  }
  def tokens = buffer.toList
}

/**
 * Provides reading of type T
 */
@implicitNotFound(msg = "Cannot find Reads or Format type class for ${T}")
trait Reads[T] {
  /**
   * Read an object from the context, returning the object read
   * @param context The context from which to read
   * @return        The object we've read
   */
  def read(context: ReadContext): T
}

object Reads {
  implicit def func2Reads[T](f: ReadContext => T): Reads[T] = new Reads[T] {
    def read(context: ReadContext) = f(context)
  }
}

/**
 * Provides writing of type T
 */
@implicitNotFound(msg = "Cannot find Writes or Format type class for ${T}")
trait Writes[T] {
  /**
   * Write an object to the context
   * @param obj     The object to write
   * @param context The context to which to append
   */
  def write(obj: T, context: WriteContext)
}

object Writes {
  implicit def func2Writes[T](f: (T, WriteContext) => Unit): Writes[T] = new Writes[T] {
    def write(obj: T, context: WriteContext) = f(obj, context)
  }
}

/**
 * Provides reading for type T from a context of type R, and writing to a context of type W.
 */
trait Format[T] extends Reads[T] with Writes[T]


object WritesString {
  implicit val writesString = new Writes[String] {
    def write(s: String, c: WriteContext) = c.target.write(StringToken(s))
  }
}

object WritesInt {
  implicit val writesInt = new Writes[Int] {
    def write(i: Int, c: WriteContext) = c.target.write(IntToken(i))
  }
}

object WritesList{
  implicit def writesList[T](implicit writes: Writes[T]) = new Writes[List[T]] {
    def write(list: List[T], c: WriteContext) {
      c.target.write(OpenArr)
      list.foreach(t => writes.write(t, c))
      c.target.write(CloseArr)
    }
  }
}

object Writing {
  def write[T](obj: T, context: WriteContext)(implicit writes: Writes[T]) = writes.write(obj, context)
}