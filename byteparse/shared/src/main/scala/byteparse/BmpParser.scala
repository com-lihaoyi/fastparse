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
  import ByteUtils.LE._

  val fileHeader = P( AnyWordI /*headerType*/ ~ AnyDwordI /*size*/ ~
    AnyWord ~ AnyWord ~
    AnyDwordI /*offset*/ ).map(s => FileHeader.tupled(s))

  val bitmapInfoHeaderPart = P( AnyDwordI /*width*/ ~ AnyDwordI /*height*/ ~
    AnyWordI /*colorPlanes*/ ~ AnyWordI /*bitsPerPixel*/ ~
    AnyDwordI /*compression*/ ~ AnyDwordI /*imageSize*/ ~
    AnyDwordI /*horzRes*/ ~ AnyDwordI /*vertRes*/ ~
    AnyDwordI /*colorUsed*/ ~ AnyDwordI /*colorsImportant*/ ).map(
    s => BitmapInfoHeader(BitmapInfoHeaderPart.tupled(s)))

  val bitmapV2HeaderPart = {
    val RgbPart = P( AnyByte.rep(exactly=4) )
    P( bitmapInfoHeaderPart ~ RgbPart.rep(exactly=3) )
  }

  val bitmapV3HeaderPart = {
    val AlphaPart = P( AnyByte.rep(exactly=4) )
    P( bitmapV2HeaderPart ~ AlphaPart )
  }

  val bitmapV4HeaderPart = P( bitmapV3HeaderPart ~ AnyByte.rep(exactly=52) )

  val bitmapV5HeaderPart = P( bitmapV4HeaderPart ~ AnyByte.rep(exactly=16) )


  val bitmapInfoHeader = P( BS(40, 0, 0, 0) /* 40 bytes */ ~/ bitmapInfoHeaderPart )
  val bitmapV2Header = P( BS(52, 0, 0, 0) ~/ bitmapV2HeaderPart )
  val bitmapV3Header = P( BS(56, 0, 0, 0) ~/ bitmapV3HeaderPart )
  val bitmapV4Header = P( BS(108, 0, 0, 0) ~/ bitmapV4HeaderPart )
  val bitmapV5Header = P( BS(124, 0, 0, 0) ~/ bitmapV5HeaderPart )

  val bitmapHeader = P(bitmapInfoHeader | bitmapV2Header |
    bitmapV3Header | bitmapV4Header | bitmapV5Header)

  def bmpRow(width: Int, bitsPerPixel: Int): P[Seq[Pixel]] = {
    val bytesPerPixel = bitsPerPixel / 8
    val padding = (width * bytesPerPixel) % 4
    P( AnyByte.rep(exactly=bytesPerPixel).!.rep(exactly=width) ~ AnyByte.rep(exactly=padding) ).map(
      pixels => pixels.map(Pixel)
    )
  }

  val bmp = P( fileHeader ~ bitmapHeader.flatMap {
    case header: BitmapHeader =>
      val infoPart = header.infoPart
      bmpRow(infoPart.width, infoPart.bitsPerPixel).rep(exactly=infoPart.height).map(pixels => (header, pixels))
  } ).map{case (fileHeader, (bitmapHeader, pixels)) => Bmp(fileHeader, bitmapHeader, pixels.reverse)}
}
