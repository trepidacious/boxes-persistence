package org.rebeam.boxes.persistence

import boxes.transact.{Txn, TxnR}

import scala.collection.mutable.ListBuffer

object BufferTokenWriter {
  def apply() = new BufferTokenWriter()
}

class BufferTokenWriter extends TokenWriter {
  private val buffer = ListBuffer[Token]()
  override def write(t: Token) = buffer.append(t)
  def tokens = buffer.toList
  def close() {}
}

object BufferTokenReader {
  def apply(tokens: List[Token]) = new BufferTokenReader(tokens)
}

class BufferTokenReader(tokens: List[Token]) extends TokenReader {
  private val buffer = ListBuffer(tokens: _*)

  def peek: Token = buffer.headOption.getOrElse(throw new NoTokenException())
  def pull(): Token = if (buffer.isEmpty) {
    throw new NoTokenException()
  } else {
    buffer.remove(0)
  }
  def close() {}
}

object BufferIO {
  def toTokens[T :Writes](t: T)(implicit txn: TxnR) = {
    val w = BufferTokenWriter()
    val context = WriteContext(w, txn)
    Writing.write(t, context)
    w.tokens
  }

  def fromTokens[T: Reads](tokens: List[Token])(implicit txn: Txn): T = {
    val r = BufferTokenReader(tokens)
    val context = ReadContext(r, txn)
    Reading.read[T](context)
  }
}
