package fastparse


import utest._

/**
 * A complete, self-contained JSON parser that parses the JSON
 * but does not build an AST. Demonstrates the use of `~!` cuts
 * to provide excellent error-reporting almost for free
 */
object JsonTests extends TestSuite{
  /**
   * A very small, very simple JSON AST
   */
  object Js {
    sealed trait Val extends Any {
      def value: Any
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
  // Here is the parser

  val space         = R( CharIn(" \n").rep1 )
  val digits        = R( CharIn('0' to '9').rep1 )
  val exponent      = R( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = R( "." ~ digits )
  val integral      = R( "0" | CharIn('1' to '9') ~ digits.rep )
  val number        = R( "?".? ~ integral ~ fractional.? ~ exponent.? ).!.map(x => Js.Num(x.toDouble))

  val `null`        = R( "null" ).map(_ => Js.Null)
  val `false`       = R( "false" ).map(_ => Js.False)
  val `true`        = R( "true" ).map(_ => Js.True)

  val hexDigit      = R( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = R( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = R( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
  val string        = R( space.? ~ "\"" ~ (!"\"" ~ AnyChar | escape).rep.! ~ "\"").map(Js.Str)
  val array         = R( "[" ~! jsonExpr.rep("," ~! Pass) ~ space.? ~ "]").map(Js.Arr(_:_*))
  val pair          = R( string.map(_.value) ~! ":" ~! jsonExpr )
  val obj           = R( "{" ~! pair.rep("," ~! Pass) ~ space.? ~ "}" ).map(Js.Obj(_:_*))
  val jsonExpr: R[Js.Val]  = R(space.? ~ (obj | array | string | `true` | `false` | `null` | number) ~ space.?)

  val tests = TestSuite{
    'pass {
      def test(p: R[_], s: String) = p.parse(s) match{
        case Result.Success(v, i) =>
          val expectedIndex = s.length
          assert(i == {s; expectedIndex})
        case f: Result.Failure => throw new Exception(f.fullStack.mkString("\n"))
      }
      * - test(number, "12031.33123E-2")
      * - test(string, "\"i am a cow lol omfg\"" )
      * - test(array, """[1, 2, "omg", ["wtf", "bbq", 42]]""")
      * - test(obj, """{"omg": "123", "wtf": 456, "bbq": "789"}""")
      * - {
        val Result.Success(value, _) = jsonExpr.parse("""{"omg": "123", "wtf": 12.4123}""")
        assert(value == Js.Obj("omg" -> Js.Str("123"), "wtf" -> Js.Num(12.4123)))
      }
      * - test(jsonExpr, """
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
      def check(s: String, expectedError: String) = {
        jsonExpr.parse(s) match{
          case s: Result.Success[_] => throw new Exception("Parsing should have failed:")
          case f: Result.Failure =>
            val error = f.trace
            val expected = expectedError.trim
            assert(error == expected)
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
          jsonExpr:0 / (obj | array | string | true | false | null | number):9 ..."}\n        "
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
          jsonExpr:0 / obj:9 / "}":23 ..."firstName\""
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
          jsonExpr:0 / obj:9 / pair:10 / ":":34 ..." \"John\",\n "
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
          jsonExpr:0 / obj:9 / "}":56 ..."lastName\":"
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
          jsonExpr:0 / obj:9 / "}":154 ...": \"21 2nd "
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
          jsonExpr:0 / obj:9 / pair:438 / string:438 / "\"":455 ..."{\n        "
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
          jsonExpr:0 / obj:9 / pair:292 / jsonExpr:320 / obj:337 / pair:338 / ":":365 ..." \"home\",\n "
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
          jsonExpr:0 / obj:9 / pair:292 / jsonExpr:320 / array:321 / jsonExpr:322 / obj:339 / "}":411 ..."555-1234\n "
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
          jsonExpr:0 / obj:9 / pair:292 / jsonExpr:320 / array:321 / jsonExpr:440 / obj:457 / "}":528 ..."555-4567\n "
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
          jsonExpr:0 / obj:9 / pair:292 / jsonExpr:320 / array:321 / "]":566 ..."}\n        "
        """
      )
    }
  }
}
