
package test.fastparse

import fastparse._
import fastparse._
import utest._

object Js {
  sealed trait Val extends Any {
    def value: Any
    def apply(i: Int): Val = this.asInstanceOf[Arr].value(i)
    def apply(s: java.lang.String): Val =
      this.asInstanceOf[Obj].value.find(_._1 == s).get._2
  }
  case class Str(value: java.lang.String) extends AnyVal with Val
  case class Obj(value: (java.lang.String, Val)*) extends AnyVal with Val
  case class Arr(value: Val*) extends AnyVal with Val
  case class Num(value: Double) extends AnyVal with Val
  case object False extends Val{
    def value = false
  }
  case object True extends Val{
    def value = true
  }
  case object Null extends Val{
    def value = null
  }
}

case class NamedFunction[T, V](f: T => V, name: String) extends (T => V){
  def apply(t: T) = f(t)
  override def toString() = name

}
object Json{
  import fastparse._, NoWhitespace._
  def stringChars(c: Char) = c != '\"' && c != '\\'

  def space[$: P]         = P( CharsWhileIn(" \r\n", 0) )
  def digits[$: P]        = P( CharsWhileIn("0-9") )
  def exponent[$: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  def fractional[$: P]    = P( "." ~ digits )
  def integral[$: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

  def number[$: P] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  def `null`[$: P]        = P( "null" ).map(_ => Js.Null)
  def `false`[$: P]       = P( "false" ).map(_ => Js.False)
  def `true`[$: P]        = P( "true" ).map(_ => Js.True)

  def hexDigit[$: P]      = P( CharIn("0-9a-fA-F") )
  def unicodeEscape[$: P] = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  def escape[$: P]        = P( "\\" ~ (CharIn("\"/\\\\bfnrt") | unicodeEscape) )

  def strChars[$: P] = P( CharsWhile(stringChars) )
  def string[$: P] =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str.apply)

  def array[$: P] =
    P( "[" ~/ jsonExpr.rep(sep=","./) ~ space ~ "]").map(Js.Arr(_:_*))

  def pair[$: P] = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  def obj[$: P] =
    P( "{" ~/ pair.rep(sep=","./) ~ space ~ "}").map(Js.Obj(_:_*))

  def jsonExpr[$: P]: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )
}

/**
  * A complete, self-contained JSON parser that parses the JSON
  * but does not build an AST. Demonstrates the use of `~/` cuts
  * to provide excellent error-reporting almost for free
  */
object JsonTests extends TestSuite{
  import Json._
  val tests = Tests{
    test("pass"){
      def check(p: P[_] => P[Any], s: String) = parse(s, p) match{
        case Parsed.Success(v, i) =>
          val expectedIndex = s.length
          assert(i == expectedIndex)
        case f: Parsed.Failure => throw new Exception(f.index.toString)
      }

      test("parts"){
        test - check(number(_), "12031.33123E-2")
        test - check(string(_), "\"i am a cow lol omfg\"")
        test - check(array(_), """[1, 2, "omg", ["wtf", "bbq", 42]]""")
        test - check(obj(_), """{"omg": "123", "wtf": 456, "bbq": "789"}""")
      }
      test("jsonExpr"){
        val Parsed.Success(value, _) =
          parse("""{"omg": "123", "wtf": 12.4123}""", jsonExpr(_))

        assert(value == Js.Obj("omg" -> Js.Str("123"), "wtf" -> Js.Num(12.4123)))
      }
      test("bigJsonExpr") - check(jsonExpr(_), """
            {
                "firstName": "John",
                "lastName": "Smith",
                "age": 25,
                "address": {
                    "streetAddress": "21 2nd Street",
                    "city": "New York",
                    "state": "NY",
                    "postalCode": 10021
                },
                "phoneNumbers": [
                    {
                        "type": "home",
                        "number": "212 555-1234"
                    },
                    {
                        "type": "fax",
                        "number": "646 555-4567"
                    }
                ]
            }
      """)
    }
//    test("perf"){
//      val input = getClass.getResourceAsStream("/fastparse/test.json")
//
//      val buffer = new java.io.BufferedReader(new java.io.InputStreamReader(input))
//      import collection.JavaConverters._
//      val txt = buffer.lines.iterator().asScala.mkString("\n")
//      var count = 0
//      val startTime = System.currentTimeMillis()
//      while(startTime + 100 > System.currentTimeMillis()){
//        parse(txt).read(jsonExpr(_))
//        count += 1
//      }
//      println(txt)
//      count
//    }
    test("fail"){
      def check(s: String, expectedError: String, expectedShortError: String) = {
        parse(s, jsonExpr(_)) match{
          case s: Parsed.Success[_] => throw new Exception("Parsing should have failed:")
          case f: Parsed.Failure =>
//            val error = f.deepTrace().msg
//            val expected = expectedError.trim
//            assert(error == expected)
        }
      }
      test - check(
        """
        }
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / (obj | array | string | true | false | null | number):2:9 ..."}\n        "
        """,
        """
          (obj | array | string | true | false | null | number):9 ..."}\n        "
        """
      )
      test - check(
        """
        {
            firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / ("}" | "\""):3:13 ..."firstName\""
        """,
        """
          "}":23 ..."firstName\""
        """
      )
      test - check(
        """
        {
            "firstName" "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:2:10 / ":":3:24 ..." \"John\",\n "
        """,
        """
          ":":34 ..." \"John\",\n "
        """
      )
      test - check(
        """
        {
            "firstName": "John,
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / ("}" | ","):4:14 ..."lastName\":"
        """,
        """
          "}":56 ..."lastName\":"
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address":
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / ("}" | ","):7:32 ...": \"21 2nd "
        """,
        """
          "}":154 ...": \"21 2nd "
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers":
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:16:19 / string:16:19 / "\"":17:17 ..."{\n        "
        """,
        """
          "\"":455 ..."{\n        "
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers":
                {
                    "type" "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:11:15 / jsonExpr:12:28 / obj:13:17 / pair:13:18 / ":":14:27 ..." \"home\",\n "
        """,
        """
          ":":365 ..." \"home\",\n "
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": 212 555-1234
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:11:15 / jsonExpr:12:28 / array:12:29 / jsonExpr:12:30 / obj:13:17 / ("}" | ","):15:35 ..."555-1234\n "
        """,
        """
          "}":411 ..."555-1234\n "
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": 646 555-4567
                }
            ]
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:11:15 / jsonExpr:12:28 / array:12:29 / jsonExpr:16:19 / obj:17:17 / ("}" | ","):19:35 ..."555-4567\n "
        """,
        """
          "}":528 ..."555-4567\n "
        """
      )
      test - check(
        """
        {
            "firstName": "John",
            "lastName": "Smith",
            "age": 25,
            "address": {
                "streetAddress": "21 2nd Street",
                "city": "New York",
                "state": "NY",
                "postalCode": 10021
            },
            "phoneNumbers": [
                {
                    "type": "home",
                    "number": "212 555-1234"
                },
                {
                    "type": "fax",
                    "number": "646 555-4567"
                }
        }
        """,
        """
          jsonExpr:1:1 / obj:2:9 / pair:11:15 / jsonExpr:12:28 / array:12:29 / ("]" | ","):22:9 ..."}\n        "
        """,
        """
          "]":566 ..."}\n        "
        """
      )
    }
  }
}
