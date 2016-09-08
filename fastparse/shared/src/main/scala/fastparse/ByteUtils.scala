package fastparse

import fastparse.Utils.IsReachable
import fastparse.core.ParseCtx

import scala.collection.mutable

/**
  * Parsers and parsing-related tools that only really apply to byte parsers,
  * and not FastParse as a whole.
  */
object ByteUtils{
  def prettyBytes(bytes: Array[Byte],
                  markers: Seq[Int] = Seq(-1),
                  contextRows: Int = 8) = {
    val invalidIndices = markers.filter(
      x => x < -1 /*-1 is special case*/ || x >= bytes.length
    )
    require(
      invalidIndices.isEmpty,
      s"Cannot show indices $markers outside input bounds 0 -> ${bytes.length}"
    )

    val maxIndexWidth = math.floor(math.max(0, math.log10(99))).toInt + 1
    val gutter = 4
    val output = new StringBuffer
    val emptyGutter = " " * (maxIndexWidth + gutter)
    output.append(emptyGutter)
    output.append(0.until(math.min(16, bytes.length)).map(x => x.toString.padTo(2, ' ')).mkString(" ").trim)
    output.append('\n')

    val sortedIndices = markers.sorted
    val groupedIndices = mutable.Buffer(mutable.Buffer(sortedIndices.head))
    for (index <- sortedIndices.tail){
      if (index / 16 - groupedIndices.last.last / 16 < contextRows){
        groupedIndices.last.append(index)
      }else{
        groupedIndices.append(mutable.Buffer(index))
      }
    }


    if (groupedIndices.head.head / 16 - contextRows > 0) {
      output.append('\n')
      output.append(emptyGutter)
      output.append("...")
    }
    for(grouped <- groupedIndices){
      val startRow = grouped.head / 16 - contextRows
      val endRow = grouped.last / 16 + contextRows
      for{
        i <- startRow to endRow
        sliced = bytes.slice(i * 16, (i + 1) * 16)
        if sliced.nonEmpty
      }{

        val prettyRow = ElemTypeFormatter.ByteFormatter.prettyPrint(sliced)
        output.append('\n')
        output.append((i * 16).toString.padTo(maxIndexWidth + gutter, ' '))
        output.append(prettyRow)

        val markers = Array.fill(prettyRow.length)(' ')
        var lastMarker = -1
        for (index <- grouped if index / 16 == i && index >= 0){
          lastMarker = (index % 16) * 3
          markers(lastMarker) = '^'
        }
        if (lastMarker != -1) {
          output.append("\n")
          output.append(emptyGutter)
          output.append(new String(markers).take(lastMarker + 1))
        }
      }
      if (endRow < bytes.length / 16) {
        output.append('\n')
        output.append(emptyGutter)
        output.append("...")
      }
    }

    output.toString
  }
  private[this] type Parser[+T] = fastparse.core.Parser[T, Byte, Array[Byte]]



  class GenericIntegerParser[T](n: Int, creator: (IsReachable[Byte], Int) => T)
                               (implicit name: sourcecode.Name) extends Parser[T]{

    override def toString = name.value

    def parseRec(cfg: ParseCtx[Byte, Array[Byte]], index: Int) = {
      if (!cfg.input.isReachable(n + index - 1)) fail(cfg.failure, index)
      else success(cfg.success, creator(cfg.input, index), index + n, Set.empty, false)

    }
  }

  protected[this] def inputToByte(input: IsReachable[Byte], n: Int): Byte = input(n)
  /**
    * Parses an 8-bit signed Byte
    */
  val Int8: Parser[Byte] = new GenericIntegerParser(1, inputToByte)
  /**
    * Parses an 8-bit un-signed Byte, stuffed into a Short
    */
  val UInt8: Parser[Short] = new GenericIntegerParser(1, (input, n) =>
    (inputToByte(input, n) & 0xff).toShort
  )


  trait EndianByteParsers {

    protected[this] def inputToShort(input: IsReachable[Byte], n: Int): Short
    protected[this] def inputToInt(input: IsReachable[Byte], n: Int): Int
    protected[this] def inputToLong(input: IsReachable[Byte], n: Int): Long


    /**
      * Parses an 16-bit signed Short
      */
    val Int16: Parser[Short] = new GenericIntegerParser(2, inputToShort)
    /**
      * Parses an 32-bit signed Short
      */
    val Int32: Parser[Int] = new GenericIntegerParser(4, inputToInt)
    /**
      * Parses an 64-bit signed Short
      */
    val Int64: Parser[Long] = new GenericIntegerParser(8, inputToLong)

    /**
      * Parses an 16-bit signed Short, stuffed into an Int
      */
    val UInt16: Parser[Int] = new GenericIntegerParser(2, (input, n) =>
      inputToShort(input, n)  & 0xffff
    )

    /**
      * Parses an 32-bit signed Int, stuffed into a Long
      */
    val UInt32: Parser[Long] = new GenericIntegerParser(4, (input, n) =>
      inputToInt(input, n) & 0xffffffffl
    )

    /**
      * Parses an 32-bit signed Float
      */
    val Float32: Parser[Float] = new GenericIntegerParser(4, (input, n) =>
      java.lang.Float.intBitsToFloat(inputToInt(input, n))
    )

    /**
      * Parses an 32-bit signed Double
      */
    val Float64: Parser[Double] = new GenericIntegerParser(8, (input, n) =>
      java.lang.Double.longBitsToDouble(inputToLong(input, n))
    )
  }
  object EndianByteParsers{
    /**
      * Parsers for parsing 16, 32 and 64 bit integers in little-endian format
      */
    object LE extends EndianByteParsers {
      def inputToLong(input: IsReachable[Byte], n: Int) = {
        ((input(n+7) & 0xffL) << 56) | ((input(n+6) & 0xffL) << 48) |
        ((input(n+5) & 0xffL) << 40) | ((input(n+4) & 0xffL) << 32 ) |
        ((input(n+3) & 0xffL) << 24) | ((input(n+2) & 0xffL) << 16) |
        ((input(n+1) & 0xffL) << 8) | (input(n) & 0xffL)
      }
      def inputToInt(input: IsReachable[Byte], n: Int) = {
        ((input(n+3) & 0xff) << 24) | ((input(n+2) & 0xff) << 16) |
        ((input(n+1) & 0xff) << 8) | (input(n) & 0xff)
      }
      def inputToShort(input: IsReachable[Byte], n: Int) = {
        (((input(n+1) & 0xff) << 8) | (input(n) & 0xff)).toShort
      }
    }

    /**
      * Parsers for parsing 16, 32 and 64 bit integers in big-endian format
      */
    object BE extends EndianByteParsers {
      def inputToLong(input: IsReachable[Byte], n: Int) = {
        ((input(n) & 0xffL) << 56) | ((input(n+1) & 0xffL) << 48) |
        ((input(n+2) & 0xffL) << 40) | ((input(n+3) & 0xffL) << 32 ) |
        ((input(n+4) & 0xffL) << 24) | ((input(n+5) & 0xffL) << 16) |
        ((input(n+6) & 0xffL) << 8) | (input(n+7) & 0xffL)
      }
      def inputToInt(input: IsReachable[Byte], n: Int) = {
        ((input(n) & 0xff) << 24) | ((input(n+1) & 0xff) << 16) |
        ((input(n+2) & 0xff) << 8) | (input(n+3) & 0xff)
      }
      def inputToShort(input: IsReachable[Byte], n: Int) = {
        (((input(n) & 0xff) << 8) | (input(n+1) & 0xff)).toShort
      }
    }
  }
}
