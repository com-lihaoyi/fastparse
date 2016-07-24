package byteparse

import java.net.URL

import fastparse.{IteratorParserInput, LoggedParsedInput}

import utest._
import fastparse.allByte._
import BmpParser._

object LargeBmpIteratorTests extends TestSuite {
  val url = "https://raw.githubusercontent.com/lihaoyi/fastparse/master/byteparse/jvm/src/test/resources/lena.bmp"
  def stream = new Iterator[IndexedSeq[Byte]] {
    val stream = new URL(url).openStream()
    val buffer = new Array[Byte](100)
    var bufferLen = 0
    var isRead = false

    def readBuffer() = {
      bufferLen = stream.read(buffer)
      isRead = true
    }

    override def hasNext: Boolean =
      if (bufferLen != -1 && isRead) {
        true
      } else {
        readBuffer()
        bufferLen != -1
      }

    override def next(): IndexedSeq[Byte] = {
      if (!isRead)
        readBuffer()
      isRead = false
      buffer.take(bufferLen)
    }
  }

  val tests = TestSuite {
    'large {
      val Parsed.Success(_, i) = bmp.parseIterator(stream)
      val expectedIndex = stream.map(_.length).sum
      assert(i == expectedIndex)
    }

    'maxInnerLength {
      val loggedInput = new LoggedParsedInput[Byte](IteratorParserInput(stream)) {
        var maxInnerLength = 0
        override def logDropBuffer(index: Int) = {
          maxInnerLength = math.max(maxInnerLength, this.innerLength)
        }
      }

      val Parsed.Success(_, i) = bmp.parseInput(loggedInput)
      println(s"Size: ${stream.map(_.length).sum}")
      println(s"Max buffer length: ${loggedInput.maxInnerLength}")
    }
  }
}
