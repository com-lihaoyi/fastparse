package byteparse

import java.nio.ByteBuffer
import java.nio.ByteOrder.{LITTLE_ENDIAN => LE}

import fastparse.allByte._

object BmpParser {
  object BmpAst {

    case class FileHeader(headerType: Int, size: Int, offset: Int) // probably useless information in our case

    case class BitmapInfoHeaderPart(width: Int,       height: Int,
                                    colorPlanes: Int, bitsPerPixel: Int,
                                    compression: Int, imageSize: Int,
                                    horzRes: Int,     vertRes: Int,
                                    colorUsed: Int,   colorsImportant: Int)

    abstract class BitmapHeader(infoPart: BitmapInfoHeaderPart)

    case class BitmapInfoHeader(infoPart: BitmapInfoHeaderPart) extends BitmapHeader(infoPart)

    case class Pixel(colors: ByteSeq)

    case class Bmp(fileHeader: FileHeader, bitmapHeader: BitmapHeader, pixels: Seq[Seq[Pixel]])

  }

  import BmpAst._
  import fastparse.ByteUtils.LE._

  val fileHeader = {
    val headerType = AnyWordI
    val size = AnyDwordI
    val offset = AnyDwordI
    P( headerType ~ size ~ AnyWord ~ AnyWord ~ offset ).map(FileHeader.tupled)
  }

  val infoHeaderPart = {
    val width = AnyDwordI
    val height = AnyDwordI
    val colorPlanes = AnyWordI
    val bitsPerPixel = AnyWordI
    val compression = AnyDwordI
    val imageSize = AnyDwordI
    val horzRes = AnyDwordI
    val vertRes = AnyDwordI
    val colorUsed = AnyDwordI
    val colorsImportant = AnyDwordI
    P(
      width ~ height ~
      colorPlanes ~ bitsPerPixel ~
      compression ~ imageSize ~
      horzRes ~ vertRes ~
      colorUsed ~ colorsImportant
    ).map(
      s => BitmapInfoHeader(BitmapInfoHeaderPart.tupled(s))
    )
  }

  val v2HeaderPart = {
    val RgbPart = P( AnyByte.rep(exactly=4) )
    P( infoHeaderPart ~ RgbPart.rep(exactly=3) )
  }

  val v3HeaderPart = {
    val AlphaPart = P( AnyByte.rep(exactly=4) )
    P( v2HeaderPart ~ AlphaPart )
  }

  val v4HeaderPart = P( v3HeaderPart ~ AnyByte.rep(exactly=52) )

  val v5HeaderPart = P( v4HeaderPart ~ AnyByte.rep(exactly=16) )


  val infoHeader = P( BS(40, 0, 0, 0) /* 40 bytes */ ~/ infoHeaderPart )
  val v2Header = P( BS(52, 0, 0, 0) ~/ v2HeaderPart )
  val v3Header = P( BS(56, 0, 0, 0) ~/ v3HeaderPart )
  val v4Header = P( BS(108, 0, 0, 0) ~/ v4HeaderPart )
  val v5Header = P( BS(124, 0, 0, 0) ~/ v5HeaderPart )

  val header = P( infoHeader | v2Header |
    v3Header | v4Header | v5Header)

  def bmpRow(width: Int, bitsPerPixel: Int): P[Seq[Pixel]] = {
    val bytesPerPixel = bitsPerPixel / 8
    val padding = (width * bytesPerPixel) % 4
    P( AnyByte.rep(exactly=bytesPerPixel).!.~/.rep(exactly=width) ~/ AnyByte.rep(exactly=padding) ).map(
      pixels => pixels.map(Pixel)
    )
  }

  val bmp = P( fileHeader ~ header.flatMap {
    case header: BitmapHeader =>
      val infoPart = header.infoPart
      bmpRow(infoPart.width, infoPart.bitsPerPixel)
        .rep(exactly=infoPart.height)
        .map(pixels => (header, pixels))
    } ).map{
    case (fileHeader, (bitmapHeader, pixels)) =>
      Bmp(fileHeader, bitmapHeader, pixels.reverse)
  }
}
