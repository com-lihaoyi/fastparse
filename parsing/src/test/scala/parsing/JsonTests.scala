package parsing


import utest._
object JsonTests extends TestSuite{
  val space         = R( CharSets(" \n").rep1 )
  val digits        = R( CharSets('0' to '9').rep1 )
  val exponent      = R( CharSets("eE") ~ CharSets("+-").? ~ digits )
  val fractional    = R( "." ~ digits )
  val integral      = R( "0" | CharSets('1' to '9') ~ digits.rep )
  val number        = R( "?".? ~ integral ~ fractional.? ~ exponent.? )
  val `null`        = R( "null" )
  val `false`       = R( "false" )
  val `true`        = R( "true" )
  val hexDigit      = R( CharSets('0'to'9', 'a'to'f', 'A'to'F') )
  val unicodeEscape = R( "u" ~ hexDigit ~ hexDigit ~ hexDigit ~ hexDigit )
  val escape        = R( "\\" ~ (CharSets("\"/\\bfnrt") | unicodeEscape) )
  val string        = R( space.? ~ "\"" ~ (!"\"" ~ AnyChar | escape).rep ~ "\"")
  val array         = R( "[" ~ jsonExpr ~ ("," ~ jsonExpr).rep ~ space.? ~ "]")
  val pair          = R( string ~ ":" ~ jsonExpr )
  val obj           = R( "{" ~ pair ~ ("," ~ pair).rep ~ space.? ~ "}" )
  val jsonExpr: Parser[_] = R(space.? ~ (obj | array | string | `true` | `false` | `null` | number) ~ space.?)

  def check[T](parser: Parser[T], input: (String, Int), rhs: Res[T]) = {
    val (str, index) = input
    val parsed = parser.parse(str, index)
    assert({parser; str; parsed} == rhs)
  }
  val tests = TestSuite{
    'literal {
      def test(p: Parser[_], s: String) = p.parse(s, 0) match{
        case Res.Success(v, i) =>
          val expectedIndex = s.length
          assert(i == {s; expectedIndex})
        case f: Res.Failure => throw new Exception(f.ps.mkString("\n"))
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
  }
}
