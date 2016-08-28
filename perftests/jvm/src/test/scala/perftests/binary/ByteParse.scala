package perftests.binary

import java.nio.file.{Files, Paths}

import perftests.Utils
import utest._


object ByteParse extends TestSuite {
  val lenaRecource = getClass.getResource("/lena.bmp")
  val lenaSource = Files.readAllBytes(Paths.get(lenaRecource.toURI.getPath))
  def lenaIterator(size: Int) = lenaSource.grouped(size)
  val parser = fastparse.BmpTests.BmpParser.bmp

  val tests = TestSuite {
    'Lena {
      Utils.benchmarkAll(
        "ByteParse",
        parser,
        lenaSource, None,
        lenaIterator
      )
    }
  }
}
