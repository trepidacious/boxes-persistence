package org.rebeam.boxes.persistence

import java.io.{OutputStream, InputStream}

import boxes.transact._

import annotation.implicitNotFound
import scala.collection._
import scala.language.implicitConversions

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
 * Must be followed by none or more pairs of (DictEntry, encoded value as tokens), then a CloseDict, unless
 * link is LinkRef(id), in which case no further tokens are necessary, and the Dict is taken to be the
 * same one original defined with LinkId(id).
 * @param name  The name of the dictionary - defaults to NoName
 * @param link  The link for this dict, defaults to LinkEmpty for a standalone dict with no id or ref.
 */
case class OpenDict(name: TokenName = NoName, link: Link = LinkEmpty) extends Token

/**
 * Start an entry in a dictionary. Must be between OpenDict and CloseDict tokens, and
 * must be followed by tokens representing exactly one value, unless link is LinkRef in which
 * case it stands alone as an entry containing the referenced item (normally a Box)
 * @param key  The string key of the dict entry
 * @param link For plain dictionaries, this is always LinkEmpty. For dictionaries using links,
 *             may be a LinkId or LinkRef, normally the id used is the id of an associated Box.
 */
case class DictEntry(key: String, link: Link = LinkEmpty) extends Token

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
      case None =>
        val id = nextId
        nextId = nextId + 1
        c.put(thing, id)
        New(id)

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

  def close(): Unit
}

/**
 * Gives the strategy to be used when reading and writing data elements that support Links
 */
sealed trait LinkStrategy

/**
 * A subset of LinkStrategies that are suitable for use with strict formats like NodeFormat
 */
sealed trait NoDuplicatesLinkStrategy extends LinkStrategy

/**
 * When writing tokens, only LinkEmpty links are used, and as a result "mutable" data elements must not be duplicated -
 * there must be no two elements in different positions in the serialised graph that are mutable and equal to
 * each other ("mutable" elements are Boxes and anything that is serialised with Boxes, e.g. using NodeFormats, and such
 * elements implement equality by identicality. Note that these elements are not actually mutable, but resolve to
 * different values in different Revisions. In addition, Nodes may register Reactions when created, and so we do
 * not wish to have duplicates.).
 *
 * Immutable data may be duplicated (equal elements in different positions in the graph), and will be written to the
 * token stream in full each time encountered.
 *
 * This may be useful when reading or writing data
 * from or to systems that do not support references, for example when producing "standard" JSON data.
 * Data with no references or duplicates may also be more predictable and easier to understand.
 * When reading token data, only LinkEmpty links must be present.
 */
case object EmptyLinks extends LinkStrategy with NoDuplicatesLinkStrategy

/**
 * When writing tokens, only LinkId links are used, and as a result "mutable" data elements must not be duplicated -
 * there must be no two elements in different positions in the serialised graph that are mutable and equal to
 * each other ("mutable" elements are Boxes and anything that is serialised with Boxes, e.g. using NodeFormats, and such
 * elements implement equality by identicality. Note that these elements are not actually mutable, but resolve to
 * different values in different Revisions. In addition, Nodes may register Reactions when created, and so we do
 * not wish to have duplicates.).
 *
 * Immutable data may be duplicated (equal elements in different positions in the graph), and will be written to the
 * token stream in full each time encountered.
 *
 * This is useful when identifiers are associated with data, mostly when using
 * boxes. For example we might wish to send a json representation of the data including an id for each box, so that a
 * client could then receive updates tagged with an id when those boxes change on the server, and/or request that the
 * server makes changes to those boxes.
 * When reading token data, only LinkEmpty links must be present.
 */
case object IdLinks extends LinkStrategy with NoDuplicatesLinkStrategy

/**
 * Data elements are serialised and deserialised using LinkId the first time an element is encountered, and LinkRef
 * each subsequent time that an equal element is encountered. LinkEmpty is never used. This means that where a
 * single data elementis present at more than one position in the graph, it will be deserialised as a single data
 * element in more than one position.
 * This is most suited for data being stored for later reading and use in this library, for example saving data
 * to a file. This is the only strategy that permits duplicates.
 */
case object AllLinks extends LinkStrategy

case class WriteContext(writer: TokenWriter, txn: TxnR)

class IncorrectTokenException(m: String) extends RuntimeException(m)
class NoTokenException extends RuntimeException
class BoxCacheException(m: String) extends RuntimeException(m)
class NodeCacheException(m: String) extends RuntimeException(m)
class CacheException(m: String) extends RuntimeException(m)

trait TokenReader {

  @throws[NoTokenException]
  def peek: Token

  @throws[NoTokenException]
  def pull(): Token

  private val boxCache = new mutable.HashMap[Long, Box[_]]()

  private val cache = new mutable.HashMap[Long, Any]()

  def putCache(id: Long, thing: Any) = cache.put(id, thing) match {
    case Some(existingThing) => throw new CacheException("Already have a thing " + existingThing + " for id " + id)
    case _ =>
  }

  def getCacheOption(id: Long) = cache.get(id)
  def getCache(id: Long) = getCacheOption(id).getOrElse(throw new CacheException("No cached thing for id " + id))

  def putBox(id: Long, box: Box[_]) {
    if (boxCache.get(id).isDefined) throw new BoxCacheException("Already have a box for id " + id)
    boxCache.put(id, box)
  }

  def getBox(id: Long): Box[_] = {
    boxCache.getOrElse(id, throw new BoxCacheException("No cached box for id " + id))
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

  def close(): Unit
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

trait ReaderWriterFactory {
  def reader(input:InputStream): TokenReader
  def writer(output:OutputStream): TokenWriter
}

class IO(val factory: ReaderWriterFactory) {

  def write[T: Writes](t: T, output: OutputStream)(implicit txn: TxnR) = {
    val target = factory.writer(output)
    val context = WriteContext(target, txn)
    implicitly[Writes[T]].write(t, context)
    target.close()
  }

  def read[T: Reads](input:InputStream)(implicit txn: Txn) = {
    val source = factory.reader(input)
    val context = ReadContext(source, txn)
    val t = implicitly[Reads[T]].read(context)
    source.close()
    t
  }

  def writeNow[T: Writes](t: T, output: OutputStream)(implicit shelf: Shelf) = shelf.read(implicit txn => write(t, output))

  def readNow[T: Reads](input:InputStream)(implicit shelf: Shelf) = shelf.transact(implicit txn => read(input), ReactionBeforeCommit)

}