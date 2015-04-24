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

import annotation.implicitNotFound
import scala.collection.mutable.ListBuffer

/**
 * Provides reading of type T from a context of type C
 */
@implicitNotFound(msg = "Cannot find Reads or Format type class for ${T} in context ${C}")
trait Reads[T, C] {
  /**
   * Read an object from the context, returning the object read, and context after reading
   * @param context The context from which to read
   * @return        A pair of values, first is the object read, second is the context after reading
   */
  def read(context: C): (T, C)
}

object Reads {
  implicit def func2Reads[T, C](f: C => (T, C)): Reads[T, C] = new Reads[T, C] {
    def read(context: C) = f(context)
  }
}

/**
 * Provides writing of type T to a context of type C
 */
@implicitNotFound(msg = "Cannot find Writes or Format type class for ${T} in context ${C}")
trait Writes[T, C] {
  /**
   * Write an object to the context, returning the new context with the object appended
   * @param obj     The object to write
   * @param context The context to which to append
   * @return        The context with object appended
   */
  def write(obj: T, context: C): C
}

object Writes {
  implicit def func2Writes[T, C](f: (T, C) => C): Writes[T, C] = new Writes[T, C] {
    def write(obj: T, context: C) = f(obj, context)
  }
}

/**
 * Provides reading for type T from a context of type R, and writing to a context of type W.
 */
trait Format[T, R, W] extends Reads[T, R] with Writes[T, W]

trait TokenTargetContext {
  def write(t: Token)
}

trait TokenSourceContext {
  def peek: Token
  def pull(): Token

  def pullAndAssert(t:Token) {
    val p = pull()
    if (p != t) throw new RuntimeException("Expected " + t + ", got " + pull)
  }
}

class TokenTargetBuffer extends TokenTargetContext {
  private val buffer = ListBuffer[Token]()
  override def write(t: Token) = {
    buffer.append(t)
  }
  def tokens = buffer.toList
}

object WritesStringToString {
  implicit val writesString = new Writes[String, String] {
    def write(s: String, c: String) = c + " " + s
  }
}

object WritesIntToString {
  implicit val writesInt = new Writes[Int, String] {
    def write(i: Int, c: String) = c + " " + i
  }
}

object WritesListToString {
  implicit def writesList[T](implicit writes: Writes[T, String]) = new Writes[List[T], String] {
    def write(list: List[T], c: String) = list.foldLeft(c)((c: String, e: T) => writes.write(e, c))
  }
}


object WritesStringTokens {
  implicit val writesString = new Writes[String, TokenTargetBuffer] {
    def write(s: String, c: TokenTargetBuffer) = {
      c.write(StringToken(s))
      c
    }
  }
}

object WritesIntTokens {
  implicit val writesInt = new Writes[Int, TokenTargetBuffer] {
    def write(i: Int, c: TokenTargetBuffer) = {
      c.write(IntToken(i))
      c
    }
  }
}

object WritesListTokens{
  implicit def writesList[T](implicit writes: Writes[T, TokenTargetBuffer]) = new Writes[List[T], TokenTargetBuffer] {
    def write(list: List[T], c: TokenTargetBuffer) = {
      c.write(OpenArr)
      list.foreach(t => writes.write(t, c))
      c.write(CloseArr)
      c
    }
  }
}

object Writing {
  def write[T, C](obj: T, context: C)(implicit writes: Writes[T, C]) = writes.write(obj, context)
}