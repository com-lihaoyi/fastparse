package perftests

import java.nio.file.{Files, Paths}

import utest._


object ByteParse extends TestSuite {
  val lenaRecource = getClass.getResource("/lena.bmp")
  val lenaSource = wrapByteArray(Files.readAllBytes(Paths.get(lenaRecource.getPath)))
  def lenaIterator(size: Int) = lenaSource.grouped(size)
  val parser = byteparse.BmpParser.bmp

  val tests = TestSuite {
    'Lena {
      Utils.benchmarkAll("ByteParse",
        parser,
        Seq(lenaSource, lenaSource ++ Array(0.toByte)),
        lenaIterator)
    }
  }
}
