package parsing


import utest._

/**
 * A complete, self-contained JSON parser that parses the JSON
 * but does not build an AST. Demonstrates the use of `~!` cuts
 * to provide excellent error-reporting almost for free
 */
object JsonTests extends TestSuite{
  val space         = R( CharIn(" \n").rep1 )
  val digits        = R( CharIn('0' to '9').rep1 )
  val exponent      = R( CharIn("eE") ~ CharIn("+-").? ~ digits )
  val fractional    = R( "." ~ digits )
  val integral      = R( "0" | CharIn('1' to '9') ~ digits.rep )
  val number        = R( "?".? ~ integral ~ fractional.? ~ exponent.? )
  val `null`        = R( "null" )
  val `false`       = R( "false" )
  val `true`        = R( "true" )
  val hexDigit      = R( CharIn('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = R( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = R( "\\" ~ (CharIn("\"/\\bfnrt") | unicodeEscape) )
  val string        = R( space.? ~ "\"" ~ (!"\"" ~ AnyChar | escape).rep ~ "\"")
  val array         = R( ("[" ~! jsonExpr) ~ ("," ~! jsonExpr).rep ~ space.? ~ "]")
  val pair          = R( string ~! ":" ~! jsonExpr )
  val obj           = R( "{" ~! pair ~ ("," ~! pair).rep ~ space.? ~ "}" )
  val jsonExpr: R0  = R(space.? ~ (obj | array | string | `true` | `false` | `null` | number) ~ space.?)

  val tests = TestSuite{
    'pass {
      def test(p: R[_], s: String) = p.parse(s) match{
        case Result.Success(v, i, cut) =>
          val expectedIndex = s.length
          assert(i == {s; expectedIndex})
        case f: Result.Failure => throw new Exception(f.fullStack.mkString("\n"))
      }
      * - test(number, "12031.33123E-2")
      * - test(string, "\"i am a cow lol omfg\"" )
      * - test(array, """[1, 2, "omg", ["wtf", "bbq", 42]]""")
      * - test(obj, """{"omg": "123", "wtf": 456, "bbq": "789"}""")
      * - test(jsonExpr, """{"omg": 1, "wtf": 12.4123}  """)
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
          jsonExpr:0 / obj:9 / pair:10 / string:10 / "\"":23 ..."firstName\""
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
