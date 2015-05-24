package org.rebeam.boxes.persistence

import boxes.transact.Box
import scala.language.implicitConversions

private object BoxFormatUtils {
  def write[T](box: Box[T], c: WriteContext, linkStrategy: LinkStrategy, writes: Writes[T]) {
    val id = box.id()
    linkStrategy match {

      case UseLinks =>
        if (c.writer.isBoxCached(id)) {
          c.writer.write(BoxToken(LinkRef(id)))
        } else {
          c.writer.write(BoxToken(LinkId(id)))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

      case NoLinks =>
        c.writer.write(BoxToken(LinkEmpty))
        writes.write(box.get()(c.txn), c)

      case NoLinksOrDuplicates =>
        if (c.writer.isBoxCached(id)) {
          throw new BoxCacheException("Box id " + id + " was already cached, but boxLinkStrategy is NoLinksOrDuplicates")
        } else {
          c.writer.write(BoxToken(LinkEmpty))
          c.writer.cacheBox(id) //Mark box as cached
          writes.write(box.get()(c.txn), c)
        }

    }
  }

  def read[T](c: ReadContext, linkStrategy: LinkStrategy, reads: Reads[T]) = {
    c.reader.pull() match {
      case BoxToken(link) =>
        link match {
          case LinkEmpty => c.txn.create(reads.read(c))
          case LinkId(id) =>
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkId (" + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            val box = c.txn.create(reads.read(c))
            c.reader.putBox(id, box)
            box

          case LinkRef(id) =>
            if (linkStrategy == NoLinks || linkStrategy == NoLinksOrDuplicates) {
              throw new BoxCacheException("Found a Box LinkRef( " + id + ") but boxLinkStrategy is " + linkStrategy)
            }
            c.reader.getBox(id).asInstanceOf[Box[T]]
        }

      case _ => throw new IncorrectTokenException("Expected BoxToken at start of Box[_]")
    }
  }
}

object BoxFormatsNoLinksOrDuplicates {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinksOrDuplicates, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinksOrDuplicates, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinksOrDuplicates, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinksOrDuplicates, reads)
  }
}

object BoxFormatsNoLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinks, writes)
  }
  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinks, reads)
  }
  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, NoLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, NoLinks, reads)
  }
}

object BoxFormatsUseLinks {
  implicit def writesBox[T](implicit writes: Writes[T]): Writes[Box[T]] = new Writes[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, UseLinks, writes)
  }

  implicit def readsBox[T](implicit reads: Reads[T]): Reads[Box[T]] = new Reads[Box[T]] {
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, UseLinks, reads)
  }

  implicit def boxFormat[T](implicit reads: Reads[T], writes: Writes[T]): Format[Box[T]] = new Format[Box[T]] {
    def write(box: Box[T], writeContext: WriteContext) = BoxFormatUtils.write(box, writeContext, UseLinks, writes)
    def read(readContext: ReadContext) = BoxFormatUtils.read(readContext, UseLinks, reads)
  }
}
