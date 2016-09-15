package perftests.binary

import java.nio.file.{Files, Paths}

import perftests.Utils
import utest._


object MidiParse extends TestSuite {
  val goResource = getClass.getResource("/go.mid")
  val goSource = Files.readAllBytes(Paths.get(goResource.toURI.getPath))
  def goIterator(size: Int) = goSource.grouped(size).map(fastparse.byte.all.Bytes.view)
  val parser = fastparse.byte.MidiParse.midiParser

  val tests = TestSuite {
    Utils.benchmarkAll(
      "MidiParse",
      parser,
      fastparse.byte.all.Bytes(goSource), None,
      goIterator
    )
  }
}
