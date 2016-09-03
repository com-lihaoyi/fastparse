package fastparse

import java.nio.file.{Files, Paths}
import javax.sound.midi.MidiSystem

import utest._


object MidiTests extends TestSuite{
  def readResourceBytes(file: String) = {
    Files.readAllBytes(Paths.get(getClass.getResource(file).toURI.getPath))
  }

  def hexBytes(bytes: Array[Byte]) = {
    println(" \t" + 0.until(16).map(x => " " * (if (x >= 10) 0 else 1) + x).mkString(" "))
    println()

    println(
      bytes
        .take(512)
        .grouped(16)
        .zipWithIndex
        .map{case (v, i) => (i * 16) + "\t" + ElemTypeFormatter.ByteFormatter.prettyPrint(v)}
        .mkString("\n")
    )
  }

  val tests = TestSuite{
    'canon{
      import Midi._
      val bytes = readResourceBytes("/canon.mid")
      val parsed = MidiParse.midiParser.parse(bytes).get.value
      println(parsed.tracks.map(_.length))
      val expectedTrack0 = Seq(
        (0, MetaEvent.TimeSignature(4, 2, 24, 8)),
        (0, MetaEvent.KeySignature(0, false)),
        (0, MetaEvent.TimeSignature(4, 2, 24, 8)),
        (0, MetaEvent.Tempo(750000)),
        (1, MetaEvent.EndOfTrack)
      )
      val channels1 = parsed.tracks(1).collect{ case (dt, MidiEvent(channel, _)) => channel}

      assert(
        parsed.format == 1,
        parsed.tickDiv == Midi.TickDiv.Metric(256),
        parsed.tracks.length == 2,
        parsed.tracks(0) == expectedTrack0,
        // This is a simple midi with only one channel
        channels1.forall(_ == 0),
        parsed.tracks(1).length == 293
      )

      parsed.tracks(1).foreach(println)
    }
    'chronoTrigger{
      import Midi._
      val bytes = readResourceBytes("/ctend.mid")
      val parsed = MidiParse.midiParser.parse(bytes).get.value

      val expectedTrack0 = Seq(
        (0, MetaEvent.TimeSignature(1,2,24,8)),
        (0, MetaEvent.KeySignature(7,false)),
        (0, MetaEvent.Tempo(495867)),
        (120, MetaEvent.TimeSignature(4,2,24,8)),
        (37920, MetaEvent.KeySignature(-1,false)),
        (23040, MetaEvent.Tempo(472440)),
        (0, MetaEvent.EndOfTrack)
      )
      assert(
        parsed.format == 1,
        parsed.tickDiv == Midi.TickDiv.Metric(120),
        parsed.tracks.length == 19,
        // Compare first item separately since == does not work
        // on Array[Byte]
        parsed.tracks(0)(0)._2.isInstanceOf[SysExEvent.Message],
        parsed.tracks(0).drop(1) == expectedTrack0
      )
      parsed.tracks(0).foreach(println)
      assert()

    }
    'tonghua{
      import Midi._
      val bytes = readResourceBytes("/tonghua.mid")

      val parsed = MidiParse.midiParser.parse(bytes).get.value
      assert(
        parsed.format == 1,
        parsed.tickDiv == Midi.TickDiv.Metric(352),
        parsed.tracks.length == 2,
        parsed.tracks.map(_.length) == Seq(1514, 894)
      )

    }
  }

}
