package org.rebeam.boxes.persistence


import java.io.Writer

import org.rebeam.boxes.persistence.json.JsonUtils

import scala.collection.mutable.Stack


class JsonTokenWriter(writer: Writer, pretty: Boolean = false) extends TokenWriter {

  private val tokens = Stack[Token]()
  private var previousToken:Option[Token] = None

  //We need to output a comma before a Prim, OpenDict, OpenArr or DictEntry, IFF we
  //have a list open, and our previous token is not the actual OpenArr (which
  //means we are the first entry in the Arr).
  private def commaNeeded = {
    tokens.headOption match {
      case Some(OpenArr(name)) => previousToken != Some(OpenArr(name))
      case Some(OpenDict(name, link)) => previousToken match {
          case Some(OpenDict(_, _)) => false
          case Some(DictEntry(_, _)) => false
          case Some(BoxToken(_)) => false
          case _ => true
        }
      case _ => false
    }
  }

  private def commaIfNeeded() = if (commaNeeded) print(",") else println()

  private def printPrim[P](p:P) {
    commaIfNeeded()
    print("" + p)
  }

  private def quoted(s: String) = JsonUtils.quote(s)

  private def print(s:String) = writer.write(s)

  private def println() = if (pretty) {
    writer.write("\n")
    Range(0, tokens.count(t => t match {
      case OpenDict(_,_) => true
      case OpenArr(_) => true
      case _ => false
    })).foreach{i => writer.write("  ")}
  }

  def write(t: Token) {
    t match {
      case OpenDict(name, link) =>
        commaIfNeeded()
        print("{")
        tokens.push(t)
        name match {
          case SignificantName(_) => throw new IncorrectTokenException("Cannot use OpenDict with " + name + " in json output")
          case _ => {}
        }
        link match {
          case LinkEmpty => {}
          case _ => throw new IncorrectTokenException("Cannot use OpenDict with " + link + " in json output")
        }

      case DictEntry(name, link) =>
        commaIfNeeded()
        link match {
          case LinkEmpty => {}
          case LinkId(id) =>
            print(quoted("_" + name + "_id") + ":" + id + ",")
            println()
          case LinkRef(id) => new IncorrectTokenException("Cannot use DictEntry with LinkRef in json output, found ref to " + id)
        }
        println()
        print(quoted(name) + ":")

      case CloseDict =>
        tokens.pop() match {
          case OpenDict(name, link) => {
            println()
            print("}")
          }
          case _ => throw new RuntimeException("Mismatched CloseObj token")
        }


      //All primitives can be represented directly in Json. Note that numeric types
      //are distinguished by location
      case BooleanToken(p)   => printPrim(p)
      case IntToken(p)    => printPrim(p)
      case DoubleToken(p)    => printPrim(p)
      case StringToken(p)   => printPrim(quoted(p))
      case LongToken(p)    => printPrim(p)
      case FloatToken(p)    => printPrim(p)

      case OpenArr(name) =>
        name match {
          case SignificantName(_) => throw new IncorrectTokenException("Cannot use " + name + " in json output")
          case _ => {}
        }
        commaIfNeeded()
        print("[")
        tokens.push(t)
        println()

      case CloseArr =>
        tokens.pop() match {
          case OpenArr(_) =>
            println()
            print("]")

          case _ => throw new RuntimeException("Mismatched CloseArr token")
        }

      case BoxToken(link) => throw new IncorrectTokenException("Cannot use " + t + " in json output")

      case NoneToken => {
        print("null")
        println()
      }

    }
    previousToken = Some(t)
  }

  def close() {
    writer.flush
    writer.close
  }

}

//
//class JSONTokenReader(reader: Reader, aliases: ClassAliases) extends TokenReader {
//  private val parser = JsonParser(reader)
//
//  private var nextToken: Option[Token] = None
//
//  def peek = {
//    nextToken.getOrElse{
//      val t = pullToken()
//      nextToken = Some(t)
//      t
//    }
//  }
//  def pull() = {
//    val t = peek
//    if (t != End) nextToken = None
//    t
//  }
//
//  private def pullClassField() = {
//    val classToken = parser.pull
//    classToken match {
//      case JsonParser.StringVal(alias) => aliases.forAlias(alias)
//      case _ => throw new RuntimeException("Unexpected type field value, not a string: " + classToken)
//    }
//  }
//
//  private def pullInt() = {
//    val token = parser.pull
//    token match {
//      case JsonParser.IntVal(i) => i
//      case _ => throw new RuntimeException("Unexpected int field value, not an int: " + token)
//    }
//  }
//
//  private def pullDouble() = {
//    val token = parser.pull
//    token match {
//      case JsonParser.DoubleVal(i) => i
//      case _ => throw new RuntimeException("Unexpected double field value, not a double: " + token)
//    }
//  }
//
//  private def pullLink() = {
//    val l = parser.peek
//    l match {
//      case JsonParser.FieldStart("_id_") => {
//        parser.pull
//        LinkId(pullInt().intValue)
//      }
//      case JsonParser.FieldStart("_ref_") => {
//        parser.pull
//        LinkRef(pullInt().intValue)
//      }
//      case _ => LinkEmpty
//    }
//  }
//
//  private def pullSecondClassPrim[P](clazz: Class[P]) = {
//    val valToken = parser.pull
//    if (valToken != JsonParser.FieldStart("_val_")) throw new RuntimeException("Unexpected val token for second class primitive: " + valToken)
//
//    val t = if (clazz == LongCodec.clazz) LongToken(pullInt().longValue)
//    else if (clazz == FloatCodec.clazz) FloatToken(pullDouble().floatValue)
//    else throw new RuntimeException("Unexpected second class primitive clazz: " + clazz)
//
//    val closeToken = parser.pull
//    if (closeToken != JsonParser.CloseObj) throw new RuntimeException("Unexpected close token for second class primitive: " + closeToken)
//
//    t
//  }
//
//  private def pullToken(): Token = {
//    parser.pull match {
//      case JsonParser.OpenObj => {
//        val t = parser.pull
//        t match {
//          case JsonParser.FieldStart("_type_") => {
//            val clazz = pullClassField()
//            val link = pullLink()
//            OpenObj(clazz, link)
//          }
//          case JsonParser.FieldStart("_val_type_") => {
//            val clazz = pullClassField()
//            pullSecondClassPrim(clazz)
//          }
//          case _ => throw new RuntimeException("Unexpected first token in Obj, not a type or valType: " + t)
//        }
//      }
//      case JsonParser.CloseObj => CloseObj
//      case JsonParser.FieldStart(name) => OpenField(name)
//      case JsonParser.End => End
//      case JsonParser.StringVal(value) => StringToken(value)
//      case JsonParser.IntVal(value) => IntToken(value.intValue)
//      case JsonParser.DoubleVal(value) => DoubleToken(value)
//      case JsonParser.BoolVal(value) => BooleanToken(value)
//      case JsonParser.NullVal => throw new RuntimeException("Unexpected null token")
//      case JsonParser.OpenArr => OpenArr
//      case JsonParser.CloseArr => CloseArr
//    }
//  }
//
//  override def close() {
//    super.close()
//    reader.close()
//  }
//
//}
//
//object JSONDataFactory extends DataFactory {
//  def reader(input:InputStream, aliases:ClassAliases) = new JSONTokenReader(new InputStreamReader(input, "UTF-8"), aliases)
//  def writer(output:OutputStream, aliases:ClassAliases) = new JSONTokenWriter(new OutputStreamWriter(output, "UTF-8"), aliases)
//}
//
//class JSONIO(aliases:ClassAliases) extends IO(JSONDataFactory, aliases) {
//  def write(t: Any): String = {
//    val s = new StringWriter()
//    val w = new JSONTokenWriter(s, aliases)
//    Box.transact {
//      codecByClass.write(t, w)
//      w.close
//    }
//    s.toString()
//  }
//
//  def read(s: String) = {
//    //Decode, so we run as a transaction, AND reactions are handled properly
//    Box.decode {
//      val source = new JSONTokenReader(new StringReader(s), aliases)
//      val t = codecByClass.read(source)
//      source.close
//      t
//    }
//  }
//
//  def readDBO(dbo: Any) = {
//    val r = MongoTokens.toTokens(dbo, aliases)
//    //Decode, so we run as a transaction, AND reactions are handled properly
//    Box.decode {
//      val t = codecByClass.read(r)
//      r.close
//      t
//    }
//  }
//
//  def writeDBO(t: Any) = {
//    val w = new StoringTokenWriter
//    Box.transact {
//      codecByClass.write(t, w)
//      w.close()
//    }
//    MongoTokens.toDBO(new StoringTokenReader(w.tokens:_*), aliases)
//  }
//
//}
//
//object JSONIO {
//  def apply(aliases:ClassAliases = new ClassAliases): JSONIO = new JSONIO(aliases)
//}
