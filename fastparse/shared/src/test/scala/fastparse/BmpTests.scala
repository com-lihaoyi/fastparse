package fastparse

import scala.collection.mutable.ArrayBuffer
import fastparse.byte._
import utest._

/*
  The basic parser of BMP format https://en.wikipedia.org/wiki/BMP_file_format .

  https://en.wikipedia.org/wiki/BMP_file_format#/media/File:BMPfileFormat.png
  It covers only main cases, where image contains only "Bitmap File Header", "DIB HEADER" and "Image data"
  without gaps and optional fields.
 */

object BmpTests extends TestSuite {

  object BmpParse {
    object BmpAst {

      case class FileHeader(headerType: Int, size: Int, offset: Int) // probably useless information in our case

      case class BitmapInfoHeaderPart(width: Int,       height: Int,
                                      colorPlanes: Int, bitsPerPixel: Int,
                                      compression: Int, imageSize: Int,
                                      horzRes: Int,     vertRes: Int,
                                      colorUsed: Int,   colorsImportant: Int)

      abstract class BitmapHeader(infoPart: BitmapInfoHeaderPart)

      case class BitmapInfoHeader(infoPart: BitmapInfoHeaderPart) extends BitmapHeader(infoPart)

      case class Pixel(colors: Array[Byte])

      case class Bmp(fileHeader: FileHeader, bitmapHeader: BitmapHeader, pixels: Seq[Seq[Pixel]])

    }

    import BmpAst._
    import LE._

    val fileHeader = {
      val headerType = UInt16
      val size = Int32
      val offset = Int32
      P( headerType ~ size ~ Word16 ~ Word16 ~ offset ).map(FileHeader.tupled)
    }

    val infoHeaderPart = {
      val width = Int32
      val height = Int32
      val colorPlanes = UInt16
      val bitsPerPixel = UInt16
      val compression = Int32
      val imageSize = Int32
      val horzRes = Int32
      val vertRes = Int32
      val colorUsed = Int32
      val colorsImportant = Int32
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
      val RgbPart = P( Word32 )
      P( infoHeaderPart ~ RgbPart.rep(exactly=3) )
    }

    val v3HeaderPart = {
      val AlphaPart = P( Word32 )
      P( v2HeaderPart ~ AlphaPart )
    }

    val v4HeaderPart = P( v3HeaderPart ~ AnyBytes(52) )

    val v5HeaderPart = P( v4HeaderPart ~ AnyBytes(16) )


    val infoHeader = P( BS(40, 0, 0, 0) /* 40 bytes */ ~/ infoHeaderPart )
    val v2Header = P( BS(52, 0, 0, 0) ~/ v2HeaderPart )
    val v3Header = P( BS(56, 0, 0, 0) ~/ v3HeaderPart )
    val v4Header = P( BS(108, 0, 0, 0) ~/ v4HeaderPart )
    val v5Header = P( BS(124, 0, 0, 0) ~/ v5HeaderPart )

    val header = P( infoHeader | v2Header | v3Header | v4Header | v5Header)

    def bmpRow(width: Int, bitsPerPixel: Int): P[Seq[Pixel]] = {
      val bytesPerPixel = bitsPerPixel / 8
      val padding = (width * bytesPerPixel) % 4
      P( AnyBytes(bytesPerPixel).!.~/.rep(exactly=width) ~/ AnyBytes(padding) ).map(
        pixels => pixels.map(Pixel)
      )
    }

    val bmp = P(
      for{
        fileHeaderData <- fileHeader
        headerData <- header
        pixels <- bmpRow(headerData.infoPart.width, headerData.infoPart.bitsPerPixel)
                    .rep(exactly=headerData.infoPart.height)
      }yield Bmp(fileHeaderData, headerData, pixels.reverse)
    )
  }

  import BmpParse._
  import BmpParse.BmpAst._

  val tests = TestSuite {
    def compareBmps(bmp1: Bmp, bmp2: Bmp): Boolean ={
      bmp1.fileHeader == bmp2.fileHeader &&
        bmp1.bitmapHeader == bmp2.bitmapHeader &&
        bmp1.pixels.map(_.map(_.colors.deep)) == bmp2.pixels.map(_.map(_.colors.deep))
    }

    'wiki {
      /* These tests were taken from wiki page https://en.wikipedia.org/wiki/BMP_file_format */
      'example1 {
        val file1 = hexBytes(
          /*file header*/ "42 4d  46 00 00 00  00 00  00 00  36 00 00 00 " +
            /*bitmap header*/ "28 00 00 00  02 00 00 00  02 00 00 00  01 00  18 00  " +
            "00 00 00 00  10 00 00 00  13 0b 00 00  13 0b 00 00  " +
            "00 00 00 00  00 00 00 00" +
            /*pixels*/  "00 00 ff  ff ff ff  00 00  ff 00 00  00 ff 00  00 00")



        val expected = Bmp(
          FileHeader(19778, 70, 54),
          BitmapInfoHeader(BitmapInfoHeaderPart(2, 2, 1, 24, 0, 16, 2835, 2835, 0, 0)),
          ArrayBuffer(ArrayBuffer(
            Pixel(BS(0xff, 0, 0)),
            Pixel(BS(0, 0xff, 0))),
            ArrayBuffer(
              Pixel(BS(0, 0, 0xff)),
              Pixel(BS(0xff, 0xff, 0xff))
            )
          )
        )

        val Parsed.Success(bmp1, _) = bmp.parse(file1)
        assert(compareBmps(bmp1, expected))

        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val Parsed.Success(bmp1, _) = bmp.parseIterator(file1.grouped(chunkSize))
          assert(compareBmps(bmp1, expected))
        }
      }

      'example2 {
        val file1 = hexBytes(
          /*file header*/ "42 4d  9A 00 00 00  00 00  00 00  7A 00 00 00 " +
            /*bitmap header*/ "6C 00 00 00  04 00 00 00  02 00 00 00  01 00  20 00  " +
            "03 00 00 00  20 00 00 00  13 0B 00 00  13 0B 00 00 " +
            "00 00 00 00  00 00 00 00  00 00 FF 00  00 FF 00 00 " +
            "FF 00 00 00  00 00 00 FF  20 6E 69 57 " + "00" * 36 +
            "00 00 00 00  00 00 00 00  00 00 00 00" +
            /*pixels*/  "FF 00 00 7F  00 FF 00 7F  00 00 FF 7F  00 00 FF 7F " +
            "FF 00 00 FF  00 FF 00 FF  00 00 FF FF  FF FF FF FF")


        val expected = Bmp(FileHeader(19778, 154, 122),
          BitmapInfoHeader(BitmapInfoHeaderPart(4, 2, 1, 32, 3, 32, 2835, 2835, 0, 0)),
          ArrayBuffer(ArrayBuffer(
            Pixel(BS(0xff, 0, 0, 0xff)),
            Pixel(BS(0, 0xff, 0, 0xff)),
            Pixel(BS(0, 0, 0xff, 0xff)),
            Pixel(BS(0xff, 0xff, 0xff, 0xff))),
            ArrayBuffer(
              Pixel(BS(0xff, 0, 0, 0x7f)),
              Pixel(BS(0, 0xff, 0, 0x7f)),
              Pixel(BS(0, 0, 0xff, 0x7f)),
              Pixel(BS(0, 0, 0xff, 0x7f))
            )
          )
        )

        val Parsed.Success(bmp2, _) = bmp.parse(file1)
        assert(compareBmps(bmp2, expected))

        for(chunkSize <- Seq(1, 4, 16, 64, 256, 1024)){
          val Parsed.Success(bmp2, _) = bmp.parseIterator(file1.grouped(chunkSize))
          assert(compareBmps(bmp2, expected))
        }
      }
    }
  }
}
