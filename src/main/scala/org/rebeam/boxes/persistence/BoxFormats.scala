package org.rebeam.boxes.persistence

import boxes.transact.Box
import scala.language.implicitConversions

private object BoxFormatUtils {
  def write[T](box: Box[T], c: WriteContext, linkStrategy: LinkStrategy, writes: Writes[T]) {
    val id = box.id()
    linkStrategy match {

      case AllLinks =>
        if (c.writer.isBoxCached(id)) {
          c.writer.write(BoxToken(LinkRef(id)))
        } else {
          c.writer.write(BoxToken(LinkId(id)))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

      case EmptyLinks =>
        if (c.writer.isBoxCached(id)) {
          throw new BoxCacheException("Box id " + id + " was already cached, but boxLinkStrategy is EmptyLinks")
        } else {
          c.writer.write(BoxToken(LinkEmpty))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

      case IdLinks =>
        if (c.writer.isBoxCached(id)) {
          throw new BoxCacheException("Box id " + id + " was already cached, but boxLinkStrategy is IdLinks")
        } else {
          c.writer.write(BoxToken(LinkId(id)))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

    }
  }

  def read[T](c: ReadContext, linkStrategy: LinkStrategy, reads: Reads[T]) = {
    c.reader.pull() match {
      case BoxToken(link) =>
        link match {
          case LinkEmpty =>
            if (linkStrategy == IdLinks || linkStrategy == AllLinks) {
              throw new BoxCacheException("Found a Box LinkEmpty but boxLinkStrategy is " + linkStrategy)
            }
            c.txn.create(reads.read(c))

          case LinkId(id) =>
            if (linkStrategy == EmptyLinks) {
              throw new BoxCacheException("Found a Box LinkId (" + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            val box = c.txn.create(reads.read(c))
            c.reader.putBox(id, box)
            box

          case LinkRef(id) =>
            if (linkStrategy == IdLinks || linkStrategy == EmptyLinks) {
              throw new BoxCacheException("Found a Box LinkRef( " + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            c.reader.getBox(id).asInstanceOf[Box[T]]
        }

      case _ => throw new IncorrectTokenException("Expected BoxToken at start of Box[_]")
    }
  }
}

object BoxFormatsEmptyLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, EmptyLinks, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, EmptyLinks, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, EmptyLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, EmptyLinks, reads)
  }
}

object BoxFormatsIdLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, IdLinks, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, IdLinks, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, IdLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, IdLinks, reads)
  }
}

object BoxFormatsAllLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, AllLinks, writes)
  }

  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, AllLinks, reads)
  }

  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, AllLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, AllLinks, reads)
  }
}
