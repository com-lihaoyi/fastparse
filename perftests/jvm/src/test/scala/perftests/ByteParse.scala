package perftests

import java.nio.file.{Files, Paths}

import utest._


object ByteParse extends TestSuite {
  val lenaRecource = getClass.getResource("/lena.bmp")
  val lenaSource = Files.readAllBytes(Paths.get(lenaRecource.getPath))
  val tests = TestSuite {
    val parser = byteparse.BmpParser.bmp
    'Lena {
      val results = Utils.benchmark(Seq(
        () => parser.parse(lenaSource),
        () => parser.parse(lenaSource ++ Array(0.toByte))))

      println("ByteParse Benchmark")
      println(results.map(_.mkString(" ")).mkString("\n"))
    }
  }
}
