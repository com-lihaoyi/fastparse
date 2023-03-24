package fastparse
import utest._
object UtilTests extends TestSuite {
  val tests = Tests{
    test("hello1"){
      val shortTxt =
        """'
          |""".stripMargin

      val lineStarts = fastparse.internal.Util.lineNumberLookup(shortTxt)
      lineStarts.toList ==> List(0, 2)
      val input = IndexedParserInput(shortTxt)

      val pretties = for(i <- 0 to shortTxt.length) yield input.prettyIndex(i)
      val expected = Vector(
        "1:1", "1:2",
        "2:1"
      )
      assert(pretties == expected)
    }
    test("hello2"){
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

    test("unix"){
      val txt = Array(
        "def myScalaVersion = \"2.13.2\"\n",
        "\n",
        "//hello\n",
        "println(doesntExis})"
      ).mkString

      val lineStarts = fastparse.internal.Util.lineNumberLookup(txt).toList

      assert(lineStarts == List(0, 30, 31, 39))
    }

    test("carriageReturnOnly") {
      val txt = Array(
        "def myScalaVersion = \"2.13.2\"\r",
        "\r",
        "//hello\r",
        "println(doesntExis})"
      ).mkString

      val lineStarts = fastparse.internal.Util.lineNumberLookup(txt).toList

      assert(lineStarts == List(0, 30, 31, 39))
    }

    test("windows"){
      val txt = Array(
        "def myScalaVersion = \"2.13.2\"\r\n",
        "\r\n",
        "//hello\r\n",
        "println(doesntExis})"
      ).mkString

      val lineStarts = fastparse.internal.Util.lineNumberLookup(txt).toList

      assert(lineStarts == List(0, 31, 33, 42))
    }
    test("reverseWindows"){
      val txt = Array(
        "def myScalaVersion = \"2.13.2\"\n\r",
        "\n\r",
        "//hello\n\r",
        "println(doesntExis})"
      ).mkString

      val lineStarts = fastparse.internal.Util.lineNumberLookup(txt).toList

      assert(lineStarts == List(0, 31, 33, 42))
    }
  }
}
