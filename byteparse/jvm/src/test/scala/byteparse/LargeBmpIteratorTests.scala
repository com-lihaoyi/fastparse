package byteparse

import java.io.InputStream

import fastparse.IteratorParserInput
import utest._
import fastparse.allByte._
import BmpParser._

object LargeBmpIteratorTests extends TestSuite {
  def stream = new Iterator[IndexedSeq[Byte]] {
    val stream: InputStream = getClass.getResource("/lena.bmp").openStream()
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
      val loggedInput = new IteratorParserInput[Byte](stream) {
        var maxInnerLength = 0

        override def dropBuffer(index: Int): Unit = {
          maxInnerLength = math.max(maxInnerLength, this.innerLength)
          super.dropBuffer(index)
        }
      }

      val Parsed.Success(_, i) = bmp.parseInput(loggedInput)
      println(s"Size: ${stream.map(_.length).sum}")
      println(s"Max buffer length: ${loggedInput.maxInnerLength}")
    }
  }
}
