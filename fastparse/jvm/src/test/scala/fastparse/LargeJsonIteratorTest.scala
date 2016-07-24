package fastparse

import fastparse.all._
import utest._
import fastparse.JsonTests._

import scala.io.Source

object LargeJsonIteratorTest extends TestSuite {
  val url = "https://raw.githubusercontent.com/lihaoyi/fastparse/master/fastparse/jvm/src/test/resources/test.json"
  def source = Source.fromURL(url)

  val tests = TestSuite {
    'large {
      val Parsed.Success(_, i) = jsonExpr.parseIterator(source.getLines().map(_.repr))
      val expectedIndex = source.getLines().map(_.length).sum
      assert(i == expectedIndex)
    }

    'maxInnerLength {
      val loggedInput = new LoggedParsedInput[Char](IteratorParserInput(source.getLines().map(_.repr))) {
        var maxInnerLength = 0
        override def logDropBuffer(index: Int) = {
          maxInnerLength = math.max(maxInnerLength, this.innerLength)
        }
      }

      val Parsed.Success(_, i) = jsonExpr.parseInput(loggedInput)
      println(s"Size: ${source.getLines().map(_.length).sum}")
      println(s"Max buffer length: ${loggedInput.maxInnerLength}")
    }
  }
}
