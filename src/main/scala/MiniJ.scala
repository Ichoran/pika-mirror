// This file is part of Pika-Mirror.
// Copyright Rex Kerr, 2016.
// Distributed under the Apache 2 License.

package XJ.pika

import kse.flow._

package minij {

  trait FromSubstring[A] {
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): A
    def parse(s: String, i0: Int, iN: Int): A = parse(s, i0, iN, null)
    def parse(s: String): A = parse(s, 0, s.length, null)
  }
  object FromSubstring {
    class Endpoint(var index: Int) {}
  }

  trait FromJson[A] extends FromSubstring[A] {
    def parse(js: Js): A
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): A = parse(Js.parse(s, i0, iN, ep))
  }

  trait Jsable {
    def toJson: Js
    def json(sb: StringBuilder): Unit
    def toJsonString: String = { val sb = new StringBuilder; json(sb); sb.result }    
  }

  trait ToJs extends Jsable {
    def json(sb: StringBuilder) { toJson.json(sb) }
  }

  sealed trait Js extends Jsable {
    def toJson = this
  }
  object Js extends FromSubstring[Js] {
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): Js = {
      val i = 0 max i0
      if (i < iN && i < s.length)
        s.charAt(i) match {
          case '"' => JStr.parse(s, i0, iN, ep)
          case '[' => JArr.parse(s, i0, iN, ep)
          case '{' => JObj.parse(s, i0, iN, ep)
          case 't' | 'f' => JBool.parse(s, i0, iN, ep)
          case c => throw new IllegalArgumentException("Unknown JSON at "+i+": "+c)
        }
      else throw new IllegalArgumentException("No input; JSON cannot be empty")
    }
  }

  trait JBool extends Js { def value: Boolean }
  case object JTrue extends JBool { def json(sb: StringBuilder) { sb ++= "true" }; def value = true }
  case object JFalse extends JBool { def json(sb: StringBuilder) { sb ++= "false" }; def value = false }
  object JBool extends FromSubstring[JBool] {
    def apply(b: Boolean) = if (b) JTrue else JFalse
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): JBool = {
      val i = 0 max i0
      val iM = 0 max (s.length min iN)
      if (i < iM) s.charAt(i) match {
        case 't' =>
          if (i < iM - 4 && s.charAt(i+1) == 'r' && s.charAt(i+2) == 'u' && s.charAt(i+3) == 'e') {
            if (ep ne null) ep.index = i+3
            JTrue
          }
          else throw new IllegalArgumentException("Not 'true' at "+i)
        case 'f' => 
          if (i < iM - 5 && s.charAt(i+1) == 'a' && s.charAt(i+2) == 'l' && s.charAt(i+3) == 's' && s.charAt(i+4) == 'e') { 
            if (ep ne null) ep.index = i+4
            JFalse
          }
          else throw new IllegalArgumentException("Not 'false' at "+i)
        case _ => throw new IllegalArgumentException("Neither true nor false at "+i)
      }
      else throw new NoSuchElementException("Empty input when JSON boolean expected")
    }
  }

  case class JStr(value: String) extends Js {
    def json(sb: StringBuilder) { 
      sb += '"'
      var i = 0
      while (i < value.length && (value.charAt(i) match { case c if (c >= ' ' && c <= 0x7E && c != '"' && c != '\\') => true; case _ => false })) i += 1
      if (i == value.length) sb ++= value
      else {
        i = 0
        while (i < value.length) {
          value.charAt(i) match {
            case c if c == '\\' => sb ++= "\\\\"
            case c if c == '"' => sb += '\\'; sb += '"'
            case c if (c >= ' ' && c <= 0x7E) => sb += c
            case c if c == '\n' => sb ++= "\\n"
            case c if c == '\r' => sb ++= "\\r" 
            case c if c == '\t' => sb ++= "\\t"
            case c if c == '\\' => sb ++= "\\\\"
            case c if c == '\f' => sb ++= "\\f"
            case c if c == '\b' => sb ++= "\\b"
            case c => sb ++= "\\u"; sb ++= "%04x".format(c.toInt)
          }
          i += 1
        }
      }
      sb += '"'
    }
  }
  object JStr extends FromSubstring[JStr] {
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): JStr = {
      var i = 0 max (i0 min s.length)
      val iM = i max (iN min s.length)
      if (iM - i < 2 || s.charAt(i) != '"') throw new IllegalArgumentException("Not a JSON string at "+i)
      i += 1
      var j = i
      while (j < iM && (s.charAt(j) match { case '\\' => false; case '"' => if (ep ne null) ep.index = j; return JStr(s.substring(i,j)); case _ => true })) j += 1
      val sb = new StringBuilder
      while (i < iM) {
        val c = s.charAt(i)
        if (c == '"') { if (ep ne null) ep.index = i; return new JStr(sb.result) }
        else if (c == '\\') {
          i += 1
          if (i >= iM) throw new IllegalArgumentException("End of valid JSON input after \\ in string")
          s.charAt(i) match {
            case c if c == 'n' => sb += '\n'
            case c if c == 'r' => sb += '\r'
            case c if c == 't' => sb += '\t'
            case c if c == '\\'=> sb += '\\'
            case c if c == '"' => sb += '"'
            case c if c == 'f' => sb += '\f'
            case c if c == 'b' => sb += '\b'
            case c if c == 'u' =>
              if (i+4 >= iM) throw new IllegalArgumentException("End of valid JSON input inside unicode number")
              sb += java.lang.Integer.parseInt(s.substring(i+1, i+5), 16).toChar
              i += 4
            case c if c == '/' => sb += '/'
            case _ => throw new IllegalArgumentException("Unknown JSON escape at "+i)
          }
        }
        else sb += c
        i += 1
      }
      throw new IllegalArgumentException("Not a JSON string at "+i)
    }
  }

  case class JArr(values: Array[Js]) extends Js {
    def json(sb: StringBuilder) {
      sb += '['
      var i = 0
      while (i < values.length) {
        values(i).json(sb)
        i += 1
        if (i < values.length) sb += ','
      }
      sb += ']'
    }
  }
  object JArr extends FromSubstring[JArr] {
    val empty = new JArr(new Array[Js](0))
    class Maker(js0: Js) {
      private[this] var arr = new Array[Js](6)
      private[this] var idx = 1
      arr(0) = js0
      def ~(j: JArr.type): JArr = {
        new JArr(if (idx == arr.length) arr else java.util.Arrays.copyOf(arr, idx))
      }
      def ~[A](a: A)(implicit jser: MiniJ[A]): Maker = {
        if (idx >= arr.length) arr = java.util.Arrays.copyOf(arr, ((idx << 1) | idx) & 0x7FFFFFFE)
        arr(idx) = jser asJs a
        idx += 1
        this
      }
    }
    def ~(j: JArr.type) = empty
    def ~[A](a: A)(implicit jser: MiniJ[A]): Maker = new Maker(jser asJs a)
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): JArr = {
      var i = 0 max (i0 min s.length)
      var iM = i max (iN min s.length)
      val ept = if (ep ne null) ep else new FromSubstring.Endpoint(i)
      if (iM - i < 2 || s.charAt(i) != '[') throw new IllegalArgumentException("Not a JSON array at "+i)
      i += 1
      val arr = Array.newBuilder[Js]
      var need_comma = false
      while (i < iM) {
        val c = s.charAt(i)
        if (!c.isWhitespace) {
          if (c == ']') { if (ep ne null) ep.index = i; return new JArr(arr.result) }
          else if (need_comma) {
            if (c == ',') need_comma = false
            else throw new IllegalArgumentException("Bad character in JSON array at "+i+": "+c)
          }
          else {
            arr += Js.parse(s, i, iM, ept)
            i = ept.index
            need_comma = true
          }
        }
        i += 1
      }
      throw new IllegalArgumentException("Ended JSON input inside an array")
    }
  }

  case class JObj(kvs: Array[Js]) extends Js {
    def json(sb: StringBuilder) {
      sb += '{'
      var i = 0
      while (i < kvs.length-1) {
        kvs(i).json(sb)
        sb += ':'
        kvs(i+1).json(sb)
        i += 2
        if (i < kvs.length-1) sb ++= ", "
      }
      sb += '}'
    }
  }
  object JObj extends FromSubstring[JObj] {
    val empty = new JObj(new Array[Js](0))
    class Maker(key0: JStr, value0: Js) {
      private[this] var arr = new Array[Js](6)
      private[this] var idx = 2
      arr(0) = key0
      arr(1) = value0
      def ~(j: JObj.type): JObj = {
        new JObj(if (idx == arr.length) arr else java.util.Arrays.copyOf(arr, idx))
      }
      def ~[V](k: String, v: V)(implicit jv: MiniJ[V]): Maker = {
        if (idx+1 >= arr.length) arr = java.util.Arrays.copyOf(arr, ((idx << 1) | idx) & 0x7FFFFFFE)
        arr(idx) = new JStr(k)
        arr(idx+1) = jv asJs v
        idx += 2
        this
      }
    }
    def ~(j: JObj.type) = empty
    def ~[V](k: String, v: V)(implicit jv: MiniJ[V]): Maker = new Maker(new JStr(k), jv asJs v)
    def parse(s: String, i0: Int, iN: Int, ep: FromSubstring.Endpoint): JObj = {
      var i = 0 max (i0 min s.length)
      var iM = i max (iN min s.length)
      val ept = if (ep ne null) ep else new FromSubstring.Endpoint(i)
      if (iM - i < 2 || s.charAt(i) != '{') throw new IllegalArgumentException("Not a JSON object at "+i)
      i += 1
      val arr = Array.newBuilder[Js]
      var needs = 0 // 1 = comma, 2 = key, 3 = colon, 4 = value
      while (i < iM) {
        val c = s.charAt(i)
        if (!c.isWhitespace) {
          if (needs == 2) {
            arr += JStr.parse(s, i, iM, ept)
            i = ept.index
            needs = 3
          }
          else if (needs == 3) {
            if (c == ':') needs = 4
            else throw new IllegalArgumentException("Bad character in JSON object at "+i+": "+c)
          }
          else if (needs == 4) {
            arr += Js.parse(s, i, iM, ept)
            i = ept.index
            needs = 1
          }
          else {
            if (c == '}') { if (ep ne null) ep.index = i; return new JObj(arr.result) }
            else if (needs == 1) {
              if (c == ',') needs = 2
              else throw new IllegalArgumentException("Bad character in JSON object at "+i+": "+c)
            }
            else {
              arr += JStr.parse(s, i, iM, ept)
              i = ept.index
              needs = 3
            }
          }
        }
        i += 1
      }
      throw new IllegalArgumentException("Ended JSON input inside an object.")
    }
  }

  trait MiniJ[A] {
    def asJs(a: A): Js
  }
  object MiniJ {
    def serial[A](a: A)(implicit jser: MiniJ[A]): String = {
      val sb = new StringBuilder
      (jser asJs a).json(sb)
      sb.result
    }
  }

  trait MiniJStr[A] extends MiniJ[A] {
    def asJs[A](a: A): JStr
  }
  object MiniJStr {}

  trait Priority2MiniJImplicits {
    implicit def minijForAnyJsable[A <: Jsable]: MiniJ[A] = new MiniJ[A] { def asJs(a: A): Js = a.toJson }
  }
}

package object minij extends Priority2MiniJImplicits {
  implicit val booleanMiniJ: MiniJ[Boolean] = new MiniJ[Boolean] { def asJs(b: Boolean): JBool = if (b) JTrue else JFalse }
  implicit val stringMiniJ: MiniJ[String] = new MiniJ[String] { def asJs(s: String): JStr = new JStr(s) }
  implicit val identityMiniJ: MiniJ[Js] = new MiniJ[Js] { def asJs(js: Js) = js }
  implicit def inheritIdentityMiniJ[J <: Js]: MiniJ[J] = new MiniJ[J] { def asJs(j: J): Js = j }
  implicit def anyArrayMiniJ[A](implicit ev: MiniJ[A]): MiniJ[Array[A]] =
    new MiniJ[Array[A]] { def asJs(a: Array[A]) = new JArr(a.map(x => ev asJs x)) }
}
