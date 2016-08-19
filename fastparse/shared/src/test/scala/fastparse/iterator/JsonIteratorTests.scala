package fastparse.iterator

import fastparse.JsonTests._
import fastparse.all._
import utest._

object JsonIteratorTests extends TestSuite {

  def test(p: P[_], s: String) = {
    val Parsed.Success(_, i) = p.parseIterator(s.grouped(1))
    val expectedIndex = s.length
    assert(i == expectedIndex)
  }

  val tests = TestSuite {
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
}
