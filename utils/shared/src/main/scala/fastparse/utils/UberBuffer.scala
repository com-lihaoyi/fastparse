package fastparse.utils
import acyclic.file
import scala.reflect.ClassTag

/**
  * A very fast circular, growable read-write byte buffer.
  */
class UberBuffer[T: ClassTag](initSize: Int = 32){ self =>
  private[this] var data = new Array[T](initSize)
  private[this] var readPos = 0
  private[this] var writePos = 0

  def capacity = data.length
  def apply(index: Int) = {
    if (index < 0 || index >= length) throw new IndexOutOfBoundsException(
      s"UberBuffer index $index must be between 0 and $length"
    )
    data((index + readPos) % capacity)
  }
  def length = {
    if (writePos == readPos){
      0
    }else if(writePos > readPos){
      //   1 2 3
      // - - - - -
      //   r   W
      //       R
      writePos - readPos
    } else {
      // 3 4   1 2
      // - - - - -
      //   W   r
      //   Rs
      data.length - readPos + writePos
    }
  }
  private[this] def writeAvailable = {
    if (writePos == readPos){
      data.length - 1
    }else if (writePos > readPos){
      //    1 2 3 4
      //  - - - - -
      //  W R w
      data.length - writePos - 1 + readPos
    }else{
      //    1
      //  - - - - -
      //    w W R
      readPos - writePos - 1
    }
  }

  private[this] def expand() = {
    val newData= new Array[T](data.length * 2)

    if (readPos <= writePos){
      System.arraycopy(data, readPos, newData, 0, writePos - readPos)
      writePos = writePos - readPos
    }else{
      System.arraycopy(data, readPos, newData, 0, data.length - readPos)
      System.arraycopy(data, 0, newData, data.length - readPos, writePos)
      writePos = writePos + data.length - readPos
    }
    readPos = 0

    data = newData
  }

  def write(in: Array[T], offset: Int = 0, length0: Int = -1) = {
    while (writeAvailable < in.length) expand()

    val (left, right) = in.splitAt(data.length - writePos)

    left.copyToArray(data, writePos)
    right.copyToArray(data, 0)

    writePos = incr(writePos, in.length)
  }


  def slice(start: Int, end: Int) = {
    assert(end >= start, s"end:$end must be >= start:$start")
    val startClamped = math.max(0, start)
    val endClamped = math.min(length, end)
    val actualStart = (readPos + startClamped) % data.length
    val actualEnd = (readPos + endClamped) % data.length
    val output = new Array[T](endClamped - startClamped)
    if (actualEnd >= actualStart){
      System.arraycopy(data, actualStart, output, 0, actualEnd - actualStart)
    }else{
      System.arraycopy(data, actualStart, output, 0, data.length - actualStart)
      System.arraycopy(data, 0, output, data.length - actualStart, actualEnd)
    }
    output
  }
  def drop(n: Int) = {
    readPos = incr(readPos, n)
  }

  private[this] def incr(n: Int, d: Long) = {
    ((n + d) % data.length).toInt
  }
}