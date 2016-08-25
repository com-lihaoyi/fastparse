package perftests.binary

import java.nio.file.{Files, Paths}

import byteparse.classparse.ClassParser
import perftests.Utils
import utest._


object ClassParse extends TestSuite {
  val collisionResource = getClass.getResource("/CollisionJNI.class")
  val collisionSource = Files.readAllBytes(Paths.get(collisionResource.toURI.getPath))
  def collisionIterator(size: Int) = collisionSource.grouped(size)
  val parser = ClassParser.classFile

  val tests = TestSuite {
    Utils.benchmarkAll(
      "ClassParse",
      parser,
      collisionSource, None,
      collisionIterator
    )
  }
}
