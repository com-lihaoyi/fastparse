package byteparse

import java.nio.ByteBuffer
import java.nio.ByteOrder._

import fastparse.allByte._

object ByteUtils {

  object LE {
    // Little Endian format
    def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(LITTLE_ENDIAN)

    val AnyWordI = P(AnyWord.!).map(wrapByteBuffer(_).getShort & 0xffff)
    val AnyDwordI = P(AnyDword.!).map(wrapByteBuffer(_).getInt)
  }

  object BE {
    // Big Endian format
    def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(BIG_ENDIAN)

    val AnyWordI = P(AnyWord.!).map(wrapByteBuffer(_).getShort & 0xffff)
    val AnyDwordI = P(AnyDword.!).map(wrapByteBuffer(_).getInt)
    // TODO Dword should be unsigned, but the only option is to change it to Long, what seems quite bad
  }
}
