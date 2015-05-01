package org.rebeam.boxes.persistence

import boxes.transact.Txn
import boxes.transact.TxnR

import annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

sealed trait Token

case object OpenObj extends Token
case object CloseObj extends Token

case class BoxToken(id: Int) extends Token

sealed trait Prim[P] extends Token { def p: P }

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

trait TokenWriter {
  def write(t: Token)
}

case class WriteContext(writer: TokenWriter, txn: TxnR)

class IncorrectTokenException(m: String) extends RuntimeException(m)
class NoTokenException extends RuntimeException

trait TokenReader {
  @throws[NoTokenException]
  def peek: Token

  @throws[NoTokenException]
  def pull(): Token

  @throws [IncorrectTokenException]
  def pullAndAssert(t:Token) {
    val p = pull()
    if (p != t) throw new IncorrectTokenException("Expected " + t + ", got " + p)
  }

  def pullBoolean(): Boolean = {
    val t = pull()
    t match {
      case BooleanToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a BooleanToken, got " + t)
    }
  }
  def pullInt(): Int = {
    val t = pull()
    t match {
      case IntToken(s) => s
      case _ => throw new IncorrectTokenException("Expected an IntToken, got " + t)
    }
  }
  def pullLong(): Long = {
    val t = pull()
    t match {
      case LongToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a LongToken, got " + t)
    }
  }
  def pullFloat(): Float = {
    val t = pull()
    t match {
      case FloatToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a FloatToken, got " + t)
    }
  }
  def pullDouble(): Double = {
    val t = pull()
    t match {
      case DoubleToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a DoubleToken, got " + t)
    }
  }
  def pullString(): String = {
    val t = pull()
    t match {
      case StringToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a StringToken, got " + t)
    }
  }

}

case class ReadContext(reader: TokenReader, txn: Txn)

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

object Writing {
  def write[T](obj: T, context: WriteContext)(implicit writes: Writes[T]) = writes.write(obj, context)
}
object Reading {
  def read[T](context: ReadContext)(implicit reads: Reads[T]): T = reads.read(context)
}