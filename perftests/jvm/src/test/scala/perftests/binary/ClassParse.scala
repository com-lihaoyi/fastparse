package perftests.binary

import java.nio.file.{Files, Paths}

import perftests.Utils
import utest._


object ClassParse extends TestSuite {
  val collisionResource = getClass.getResource("/CollisionJNI.class")
  val collisionSource = Files.readAllBytes(Paths.get(collisionResource.toURI.getPath))
  def collisionIterator(size: Int) = collisionSource.grouped(size)
  val parser = classparse.ClassParser.classFile

  val tests = TestSuite {
    Utils.benchmarkAll(
      "ClassParse",
      parser,
      collisionSource, None,
      collisionIterator
    )
  }
}
