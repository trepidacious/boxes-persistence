package org.rebeam.boxes.persistence

import boxes.transact.{Box, Txn, TxnR}

import annotation.implicitNotFound
import scala.collection._

sealed trait Token

sealed trait TokenName

/**
 * There is no name for this token
 */
case object NoName extends TokenName

/**
 * There is a name for this token, it is not needed for disambiguation (e.g. to choose
 * an implementation of a trait or class), but can be used for presentation in a format that
 * prefers this (e.g. as a tag name for the parent tag in XML)
 * @param name  The name of the dictionary
 */
case class PresentationName(name: String) extends TokenName

/**
 * There is a name for this token, and it is needed for disambiguation (e.g. to choose
 * an implementation of a trait or class). It must be included in all formats, even those that
 * don't normally name the structure associated with the token. For example in JSON this might result in
 * a "type tag" field being set in an object, alongside the fields for dictionary entries.
 * @param name  The name of the dictionary
 */
case class SignificantName(name: String) extends TokenName

/**
 * A link, either empty, or indicating that an object can be linked to (referenced), or indicating
 * that it links to another (references it).
 */
sealed trait Link

/**
 * Link referencing another object (of same type)
 * @param id  The id of the referenced object
 */
case class LinkRef(id: Long) extends Link

/**
 * Link indicating that this object can be referenced using
 * the specified id from another object (of the same type)
 * @param id  The id of this object
 */
case class LinkId(id: Long) extends Link

/**
 * No link for this object - it doesn't reference any other object, and
 * cannot be referenced
 */
case object LinkEmpty extends Link

/**
 * Open a dictionary, which is a map from Strings to arbitrary values
 * Must be followed by none or more pairs of (DictEntry, encoded value as tokens), then a CloseDict.
 * @param name  The name of the dictionary - defaults to NoName
 */
case class OpenDict(name: TokenName = NoName, link: Link = LinkEmpty) extends Token

/**
 * Start an entry in a dictionary. Must be between OpenDict and CloseDict tokens, and
 * must be followed by tokens representing exactly one value
 * @param key  The string key of the dict entry
 */
case class DictEntry(key: String) extends Token

/**
 * Close a dictionary
 */
case object CloseDict extends Token

/**
 * Start a Box. If link is LinkEmpty or LinkId then this token must be followed
 * by an encoded value. If link is LinkRef then this box is the same Box as the referenced
 * box.
 * Note that the link ids used are unique only for Boxes within one Shelf, since they are
 * the Box.id() values of the original serialised Box. Note that they may conflict with link ids
 * used for other types.
 * @param link  The link for this box, defaults to LinkEmpty.
 */
case class BoxToken(link: Link = LinkEmpty) extends Token

sealed trait Prim[P] extends Token { def p: P }
case class BooleanToken(p: Boolean) extends Prim[Boolean]
case class IntToken(p: Int) extends Prim[Int]
case class LongToken(p: Long) extends Prim[Long]
case class FloatToken(p: Float) extends Prim[Float]
case class DoubleToken(p: Double) extends Prim[Double]
case class StringToken(p: String) extends Prim[String]

/**
 * Represents an Option value of None. Some is represented as the value itself.
 */
case object NoneToken extends Token

/**
 * Open an array, which is an ordered list of none or more arbitrary values
 * Must be followed by none or more arbitrary values encoded as tokens, then a CloseArr.
 * @param name  The name of the array - defaults to NoName
 */
case class OpenArr(name: TokenName) extends Token
case object CloseArr extends Token

/**
 * Marks the end of a stream of tokens
 */
case object End extends Token

/**
 * Result of checking for an object in the cache
 */
sealed trait CacheResult

/**
 * Object is already cached, can refer to it using the provided id
 * @param id The id under which the object was already cached
 */
case class Cached(id: Long) extends CacheResult

/**
 * Object was not already cached, it is now cached using the provided id
 * @param id The id under which the object has now been cached
 */
case class New(id:Int) extends CacheResult

trait TokenWriter {
  def write(t: Token)

  private val c = collection.mutable.Map[Any, Int]()
  private var nextId = 0

  /**
   * Try to cache a thing. The result will tell us whether the thing
   * is already cached:
   *
   *   If already cached, the CacheResult is Cached(ref), where the
   *   supplied ref can be written out in place of the object. This
   *   refers back to the previous instance with the matching id.
   *
   *   If NOT already cached, the CacheResult is New(id), where the
   *   id should be written out with the object, so that it can be
   *   referenced by future refs.
   */
  def cache(thing:Any):CacheResult = {
    c.get(thing) match {
      case None => {
        val id = nextId
        nextId = nextId + 1
        c.put(thing, id)
        New(id)
      }
      case Some(ref) => Cached(ref)
    }
  }

  private val cachedBoxIds = mutable.HashSet[Long]()

  /**
   * Cache a box
   * @param id  The id of the box to cache
   */
  def cacheBox(id: Long) {
    if (cachedBoxIds.contains(id)) throw new BoxCacheException("Box id " + id + " is already cached - don't cache it again!")
    cachedBoxIds.add(id)
  }

  /**
   * Check whether a box is already cached
   * @param id  The id of the box to check
   * @return    True if cacheBox has already been called on this id, indicating that the full contents have been
   *            written already, and a ref can be used
   */
  def isBoxCached(id: Long) = cachedBoxIds.contains(id)

}

/**
 * Gives the strategy to be used when reading and writing data elements that support Links
 */
sealed trait LinkStrategy

/**
 * Data elements must not be duplicated - they must be found at only one place in the serialised graph. No references are
 * necessary, and so they are not used (LinkEmpty is used for all BoxTokens). When reading data, it is expected to
 * be in this format. This may be useful when reading or writing data from or to systems that do not support references,
 * for example when producing "standard" JSON data. Data with no references or duplicates may also be more predictable
 * and easier to understand.
 */
case object NoLinksOrDuplicates extends LinkStrategy

/**
 * Data elements may be duplicated - they may occur in more than one place in the serialised graph. Where this occurs,
 * they will be written without links (LinkEmpty is always used), and so when deserialised they will become different
 * data elements. This is not normally what is intended.
 */
case object NoLinks extends LinkStrategy

/**
 * Data elements are serialised and deserialised using links, LinkEmpty is never used. This means that where a
 * single box is present at more than one position in the graph, it will be deserialised as a single box in more than
 * one position. This is most suited for data being stored for later reading and use in this library, for example
 * saving data to a file.
 */
case object UseLinks extends LinkStrategy

case class WriteContext(writer: TokenWriter, txn: TxnR,
                        boxLinkStrategy: LinkStrategy = UseLinks, nodeLinkStrategy: LinkStrategy = UseLinks)

class IncorrectTokenException(m: String) extends RuntimeException(m)
class NoTokenException extends RuntimeException
class BoxCacheException(m: String) extends RuntimeException(m)

trait TokenReader {

  @throws[NoTokenException]
  def peek: Token

  @throws[NoTokenException]
  def pull(): Token

  private val boxCache = new mutable.HashMap[Long, Box[_]]()

  def putBox(id: Long, box: Box[_]) {
    if (boxCache.get(id).isDefined) throw new BoxCacheException("Already have a box for id " + id)
    boxCache.put(id, box)
  }

  def getBox(id: Long): Box[_] = {
    boxCache.get(id).getOrElse(throw new BoxCacheException("No cached box for id " + id))
  }

  @throws [IncorrectTokenException]
  def pullAndAssertEquals(t:Token) {
    val p = pull()
    if (p != t) throw new IncorrectTokenException("Expected " + t + ", got " + p)
  }

  @throws [IncorrectTokenException]
  def pullAndAssert(filter: Token => Boolean) {
    val p = pull()
    if (!filter(p)) throw new IncorrectTokenException("Assertion failed on " + p)
  }

  @throws [IncorrectTokenException]
  def pullBoolean(): Boolean = {
    val t = pull()
    t match {
      case BooleanToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a BooleanToken, got " + t)
    }
  }
  @throws [IncorrectTokenException]
  def pullInt(): Int = {
    val t = pull()
    t match {
      case IntToken(s) => s
      case _ => throw new IncorrectTokenException("Expected an IntToken, got " + t)
    }
  }
  @throws [IncorrectTokenException]
  def pullLong(): Long = {
    val t = pull()
    t match {
      case LongToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a LongToken, got " + t)
    }
  }
  @throws [IncorrectTokenException]
  def pullFloat(): Float = {
    val t = pull()
    t match {
      case FloatToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a FloatToken, got " + t)
    }
  }
  @throws [IncorrectTokenException]
  def pullDouble(): Double = {
    val t = pull()
    t match {
      case DoubleToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a DoubleToken, got " + t)
    }
  }
  @throws [IncorrectTokenException]
  def pullString(): String = {
    val t = pull()
    t match {
      case StringToken(s) => s
      case _ => throw new IncorrectTokenException("Expected a StringToken, got " + t)
    }
  }

}

case class ReadContext(reader: TokenReader, txn: Txn,
                       boxLinkStrategy: LinkStrategy = UseLinks, nodeLinkStrategy: LinkStrategy = UseLinks)

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