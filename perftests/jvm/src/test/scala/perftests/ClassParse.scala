package perftests

import java.nio.file.{Files, Paths}

import byteparse.classparse.ClassParser
import utest._


object ClassParse extends TestSuite {
  val collisionResource = getClass.getResource("/CollisionJNI.class")
  val collisionSource = Files.readAllBytes(Paths.get(collisionResource.getPath))
  val tests = TestSuite {
    val parser = ClassParser.classFile
    'Collision {
      val results = Utils.benchmark(Seq(
        () => parser.parse(collisionSource),
        () => parser.parse(collisionSource ++ Array(0.toByte))))

      println("ClassParse Benchmark")
      println(results.map(_.mkString(" ")).mkString("\n"))
    }
  }
}
