package byteparse

import java.nio.ByteBuffer
import java.nio.ByteOrder._

import fastparse.allByte._

object ByteUtils {

  trait ByteFormat {
    def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer

    val AnyWordI = P(AnyWord.!).map(wrapByteBuffer(_).getShort & 0xffff)
    val AnyDwordI = P(AnyDword.!).map(wrapByteBuffer(_).getInt)
    // TODO Dword should be unsigned, but the only option is to change it to Long, what seems quite bad

    def repeatWithSize[T](p: Parser[T], sizeParser: Parser[Int] = AnyWordI): Parser[Seq[T]] =
      P( sizeParser.flatMap(l => p.rep(exactly = l)) )
  }

  object LE extends ByteFormat {
    // Little Endian format
    def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(LITTLE_ENDIAN)
  }

  object BE extends ByteFormat {
    // Big Endian format
    def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(BIG_ENDIAN)
  }
}
