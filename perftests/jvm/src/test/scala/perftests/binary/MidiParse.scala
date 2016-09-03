package perftests.binary

import java.nio.file.{Files, Paths}

import perftests.Utils
import utest._


object MidiParse extends TestSuite {
  val goResource = getClass.getResource("/canon.mid")
  val goSource = Files.readAllBytes(Paths.get(goResource.toURI.getPath))
  def goIterator(size: Int) = goSource.grouped(size)
  val parser = fastparse.MidiParse.midiParser
  println(goSource.length)
  val tests = TestSuite {
    Utils.benchmarkAll(
      "MidiParse",
      parser,
      goSource, None,
      goIterator
    )
  }
}
