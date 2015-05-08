package org.rebeam.boxes.persistence

import scala.collection.mutable.ListBuffer

class BufferTokenWriter extends TokenWriter {
  private val buffer = ListBuffer[Token]()
  override def write(t: Token) = buffer.append(t)
  def tokens = buffer.toList
}

class BufferTokenReader(tokens: List[Token]) extends TokenReader {
  private val buffer = ListBuffer(tokens: _*)

  def peek: Token = buffer.headOption.getOrElse(throw new NoTokenException())
  def pull(): Token = if (buffer.isEmpty) {
    throw new NoTokenException()
  } else {
    buffer.remove(0)
  }
}

