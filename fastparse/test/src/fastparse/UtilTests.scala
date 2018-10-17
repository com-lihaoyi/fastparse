package fastparse
import utest._
object UtilTests extends TestSuite {
  val tests = Tests{
    'hello - {
      val txt =
        """a
          |bc
          |def
          |
          |ghij
          |lmnop""".stripMargin

      val lineStarts = fastparse.internal.Util.lineNumberLookup(txt)
      lineStarts.toList ==> List(0, 2, 5, 9, 10, 15)
      val input = IndexedParserInput(txt)

      val pretties = for(i <- 0 to txt.length) yield input.prettyIndex(i)
      val expected = Vector(
        "1:1", "1:2",
        "2:1", "2:2", "2:3",
        "3:1", "3:2", "3:3", "3:4",
        "4:1",
        "5:1", "5:2", "5:3", "5:4", "5:5",
        "6:1", "6:2", "6:3", "6:4", "6:5", "6:6"
      )
      assert(pretties == expected)
    }
  }
}
