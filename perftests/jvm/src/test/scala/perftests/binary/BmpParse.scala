package perftests.binary

import java.nio.file.{Files, Paths}

import perftests.Utils
import utest._
import fastparse.byte.all._

object BmpParse extends TestSuite {
  val lenaRecource = getClass.getResource("/lena.bmp")
  val lenaSource = Files.readAllBytes(Paths.get(lenaRecource.toURI.getPath))
  def lenaIterator(size: Int) = lenaSource.grouped(size).map(Bytes.view)
  val parser = fastparse.byte.BmpTests.BmpParse.bmp

  val tests = TestSuite {
    'Lena {
      Utils.benchmarkAll(
        "ByteParse",
        parser,
        Bytes(lenaSource), None,
        lenaIterator
      )
    }
  }
}
