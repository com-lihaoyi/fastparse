package fastparse

import all._
import utest._

/**
 * A complete, self-contained JSON parser that parses the JSON
 * but does not build an AST. Demonstrates the use of `~/` cuts
 * to provide excellent error-reporting almost for free
 */
object JsonTests extends TestSuite{
  /**
   * A very small, very simple JSON AST
   */
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
  // Here is the parser
  val Whitespace = NamedFunction(" \r\n".contains(_: Char), "Whitespace")
  val Digits = NamedFunction('0' to '9' contains (_: Char), "Digits")
  val StringChars = NamedFunction(!"\"\\".contains(_: Char), "StringChars")

  val space         = P( CharsWhile(Whitespace).? )
  val digits        = P( CharsWhile(Digits))
  val exponent      = P( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = P( "." ~ digits )
  val integral      = P( "0" | CharIn('1' to '9') ~ digits.? )

  val number = P( CharIn("+-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Js.Num(x.toDouble)
  )

  val `null`        = P( "null" ).map(_ => Js.Null)
  val `false`       = P( "false" ).map(_ => Js.False)
  val `true`        = P( "true" ).map(_ => Js.True)

  val hexDigit      = P( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = P( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = P( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )

  val strChars = P( CharsWhile(StringChars) )
  val string =
    P( space ~ "\"" ~/ (strChars | escape).rep.! ~ "\"").map(Js.Str)

  val array =
    P( "[" ~/ jsonExpr.rep(sep=",".~/) ~ space ~ "]").map(Js.Arr(_:_*))

  val pair = P( string.map(_.value) ~/ ":" ~/ jsonExpr )

  val obj =
    P( "{" ~/ pair.rep(sep=",".~/) ~ space ~ "}").map(Js.Obj(_:_*))

  val jsonExpr: P[Js.Val] = P(
    space ~ (obj | array | string | `true` | `false` | `null` | number) ~ space
  )

  val tests = TestSuite{
    'pass {
      def test(p: P[_], s: String) = p.parse(s) match{
        case Parsed.Success(v, i) =>
          val expectedIndex = s.length
          assert(i == expectedIndex)
        case f: Parsed.Failure => throw new Exception(f.extra.traced.fullStack.mkString("\n"))
      }

      'parts {
        * - test(number, "12031.33123E-2")
        * - test(string, "\"i am a cow lol omfg\"")
        * - test(array, """[1, 2, "omg", ["wtf", "bbq", 42]]""")
        * - test(obj, """{"omg": "123", "wtf": 456, "bbq": "789"}""")
      }
      'jsonExpr - {
        val Parsed.Success(value, _) = jsonExpr.parse(
          """{"omg": "123", "wtf": 12.4123}"""
        )
        assert(value == Js.Obj("omg" -> Js.Str("123"), "wtf" -> Js.Num(12.4123)))
      }
      'bigJsonExpr - test(jsonExpr, """
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
    'fail{
      def check(s: String, expectedError: String, expectedShortError: String) = {
        jsonExpr.parse(s) match{
          case s: Parsed.Success[_] => throw new Exception("Parsing should have failed:")
          case f: Parsed.Failure =>
            val error = f.extra.traced.trace
            val expected = expectedError.trim
            assert(error == expected)
        }
        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          jsonExpr.parseIterator(s.grouped(chunkSize)) match {
            case s: Parsed.Success[_] => throw new Exception("Parsing should have failed:")
            case f: Parsed.Failure =>
              val error = f.msg
              val expected = expectedShortError.trim
              assert(error == expected)
          }
        }
      }
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
      * - check(
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
