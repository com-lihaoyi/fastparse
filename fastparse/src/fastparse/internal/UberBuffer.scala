package fastparse.internal


/**
  * A very fast circular, growable read-write byte buffer.
  */
class UberBuffer(initSize: Int = 32){ self =>
  private[this] var data = new Array[Char](initSize)
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
    val newData = new Array[Char](data.length * 2)

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

  def write(in: Array[Char], length0: Int) = {
    while (writeAvailable < length0) expand()

    if (writePos + length0 <= data.length){
      System.arraycopy(in, 0, data, writePos, length0)
    }else{
      val firstHalfLength = data.length - writePos
      System.arraycopy(in, 0, data, writePos, firstHalfLength)
      System.arraycopy(in, firstHalfLength, data, 0, length0 - firstHalfLength)
    }

    writePos = incr(writePos, length0)
  }


  def slice(start: Int, end: Int) = {
    assert(end >= start, s"end:$end must be >= start:$start")
    val startClamped = math.max(0, start)
    val endClamped = math.min(length, end)
    val actualStart = (readPos + startClamped) % data.length
    val actualEnd = (readPos + endClamped) % data.length
    val output = new Array[Char](endClamped - startClamped)
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