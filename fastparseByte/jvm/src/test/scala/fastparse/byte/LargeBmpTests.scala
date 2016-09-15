package fastparse.byte

import java.awt.Color
import java.awt.image.BufferedImage
import java.nio.file.{Files, Paths}
import javax.imageio.ImageIO

import utest._
import fastparse.byte.all._

object LargeBmpTests extends TestSuite {
  import BmpTests.BmpParse.BmpAst._
  import BmpTests.BmpParse._

  val tests = TestSuite {
    'lena {
      val lenaRecource = getClass.getResource("/lena.bmp")
      val Parsed.Success(lenaBmp, _) = bmp.parse(
        Bytes(Files.readAllBytes(Paths.get(lenaRecource.toURI.getPath)))
      )

      def compareBmpImg(bmp: Bmp, img: BufferedImage): Unit = {
        assert(bmp.pixels.length == img.getHeight)
        for (y <- 0 until img.getHeight) {
          assert(bmp.pixels(y).length == img.getWidth)
          for (x <- 0 until img.getWidth) {
            val color = new Color(img.getRGB(x, y))
            assert(
              color.getRed.toByte == bmp.pixels(y)(x).colors(2) &&
              color.getGreen.toByte == bmp.pixels(y)(x).colors(1) &&
              color.getBlue.toByte == bmp.pixels(y)(x).colors(0)
            )
          }
        }
      }

      def readImg(path: String) = ImageIO.read(getClass.getResource("/" + path))

      'original {
        val lenaImg = readImg("lena.bmp")
        compareBmpImg(lenaBmp, lenaImg)
      }

      'mirrored {
        val mirroredLenaImg = readImg("mirrored_lena.bmp")
        val mirroredBmp = Bmp(
          lenaBmp.fileHeader, lenaBmp.bitmapHeader, lenaBmp.pixels.map(_.reverse)
        )
        compareBmpImg(mirroredBmp, mirroredLenaImg)
      }

      'inverted {
        val invertedLenaImg = readImg("inverted_lena.bmp")
        val invertedBmp = Bmp(
          lenaBmp.fileHeader, lenaBmp.bitmapHeader,
          lenaBmp.pixels.map(_.map(p => Pixel(p.colors.map(b => (-b - 1).toByte))))
        )
        compareBmpImg(invertedBmp, invertedLenaImg)
      }
    }
  }
}
