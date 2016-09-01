package fastparse

import java.nio.file.{Files, Paths}
import javax.sound.midi.MidiSystem

import utest._

/**
  * Built based on the definitions:
  *
  * http://www.ccarh.org/courses/253/handout/smf/
  * http://www.somascape.org/midi/tech/mfile.html#midi
  */
object MidiTests extends TestSuite{
  val lenaBytes = Files.readAllBytes(Paths.get(getClass.getResource("/ctend.mid").toURI.getPath))
  case class Midi(format: Int, tickDiv: Int, tracks: Seq[Seq[(Int, TrackEvent)]])
  sealed trait TrackEvent
  sealed trait MidiEvent extends TrackEvent

  object MidiEvent{
    case class NoteOff(note: Byte, velocity: Byte) extends MidiEvent
    case class NoteOn(note: Byte, velocity: Byte) extends MidiEvent
    case class PolyphonicPressure(note: Byte, pressure: Byte) extends MidiEvent
    case class Controller(controller: Byte, value: Byte) extends MidiEvent
    case class ProgramChange(program: Byte) extends MidiEvent
    case class ChannelPressure(pressure: Byte) extends MidiEvent
    case class PitchBend(lsb: Byte, msb: Byte) extends MidiEvent
  }

  sealed trait MetaEvent extends TrackEvent
  object MetaEvent{
    case class SequenceNumber(number: Short) extends MetaEvent
    case class Text(text: String) extends MetaEvent
    case class Copyright(text: String) extends MetaEvent
    case class TrackName(text: String) extends MetaEvent
    case class InstrumentName(text: String) extends MetaEvent
    case class Lyric(text: String) extends MetaEvent
    case class Marker(text: String) extends MetaEvent
    case class CuePoint(text: String) extends MetaEvent
    case class ProgramName(text: String) extends MetaEvent
    case class DeviceName(text: String) extends MetaEvent
    case class MidiChannelPrefix(channel: Byte) extends MetaEvent
    case class MidiPort(port: Byte) extends MetaEvent
    case object EndOfTrack extends MetaEvent
    case class Tempo(temp: Int) extends MetaEvent
    case class SmpteOffset(hour: Byte, minute: Byte, seconds: Byte, frames: Byte, fractional: Byte) extends MetaEvent
    case class TimeSignature(numerator: Byte, denominator: Byte, clocks: Byte, notatedNotes: Byte) extends MetaEvent
    case class KeySignature(sf: Byte, majorKey: Boolean) extends MetaEvent
    case class SequencerSpecificEvent(data: Array[Byte]) extends MetaEvent
  }
  sealed trait SysExEvent extends TrackEvent
  object SysExEvent{
    case class Message(data: Array[Byte]) extends SysExEvent
  }
  val tests = TestSuite{
    'hello{
      import fastparse.byte._
      import BE._
      println(
        lenaBytes
          .take(256)
          .grouped(16)
          .map(ElemTypeFormatter.ByteFormatter.prettyPrint(_))
          .mkString("\n")
      )
      val header = P( hexBytes("4d 54 68 64 00 00 00 06") ~ Int16 ~ Int16 ~ Int16 ).log()
      val deltaTime = P( BytesWhile(_ < 0, min = 0) ~ AnyByte )

      val vLength: P[Int] = P( BytesWhile(b => (b & 0x80) != 0, min = 0).! ~ Int8 ).map{
        case (bytes, last) => bytes.foldLeft[Int](last)(_ << 7 + _)
      }
      val vTime: P[Int] = P( vLength )

      val midiEvent: P[MidiEvent] = {
        val NoteOff = (Int8 ~ Int8).map(MidiEvent.NoteOff.tupled)
        val NoteOn = (Int8 ~ Int8).map(MidiEvent.NoteOn.tupled)
        val PolyphonicPressure = (Int8 ~ Int8).map(MidiEvent.PolyphonicPressure.tupled)
        val Controller = (Int8 ~ Int8).map(MidiEvent.Controller.tupled)
        val ProgramChange = Int8.map(MidiEvent.ProgramChange)
        val ChannelPressure = Int8.map(MidiEvent.ChannelPressure)
        val PitchBend = (Int8 ~ Int8).map(MidiEvent.PitchBend.tupled)
        val statusByte: P[Byte] = P( ByteIn((0x80 to 0xef).map(_.toByte)).!.map(_(0)) )
        P(
          for{
            byte <- statusByte.~/
            event <- byte & 0xf0 match{
              case 0x80 => NoteOff
              case 0x90 => NoteOn
              case 0xA0 => PolyphonicPressure
              case 0xB0 => Controller
              case 0xC0 => ProgramChange
              case 0xD0 => ChannelPressure
              case 0xE0 => PitchBend

            }
          } yield event
        )
      }

      val metaEvent = {
        val varString = vLength.flatMap(x => AnyByte.rep(exactly = x).!.map(new String(_)))
        val SequenceNumber = (BS(0x02) ~ Int8 ~ Int8).map{case (x, y) => MetaEvent.SequenceNumber((x << 8 + y).toShort)}
        val Text = varString.map(MetaEvent.Text)
        val Copyright = varString.map(MetaEvent.Copyright)
        val TrackName = varString.map(MetaEvent.TrackName)
        val InstrumentName = varString.map(MetaEvent.InstrumentName)
        val Lyric = varString.map(MetaEvent.Lyric)
        val Marker = varString.map(MetaEvent.Marker)
        val CuePoint = varString.map(MetaEvent.CuePoint)
        val ProgramName = varString.map(MetaEvent.ProgramName)
        val DeviceName = varString.map(MetaEvent.DeviceName)
        val MidiChannelPrefix = (BS(0x01) ~ Int8).map(MetaEvent.MidiChannelPrefix)
        val MidiPort = (BS(0x01) ~ Int8).map(MetaEvent.MidiPort)
        val EndOfTrack = wspByteSeq(BS(0x00)).map(_ => MetaEvent.EndOfTrack)
        val Tempo = (BS(0x03) ~ Int8 ~ Int8 ~ Int8).map{case (a, b, c) => MetaEvent.Tempo(a << 16 + b << 8 + c)}
        val SmpteOffset = (BS(0x05) ~ Int8 ~ Int8 ~ Int8 ~ Int8 ~ Int8).map(MetaEvent.SmpteOffset.tupled)
        val TimeSignature = (BS(0x04) ~ Int8 ~ Int8 ~ Int8 ~ Int8).map(MetaEvent.TimeSignature.tupled)
        val KeySignature = (BS(0x02) ~ Int8 ~ Int8).map{case (x, y) => MetaEvent.KeySignature(x, y != 0)}
        val SequencerSpecificEvent = vLength.flatMap(x => AnyByte.rep(exactly = x).!).map(MetaEvent.SequencerSpecificEvent)

        P(
          for{
            _ <- BS(0xFF) ~/ Pass
            `type` <- Int8
            length <- Int8
            result <- `type` match {
              case 0x00 => SequenceNumber
              case 0x01 => Text
              case 0x02 => Copyright
              case 0x03 => TrackName
              case 0x04 => InstrumentName
              case 0x05 => Lyric
              case 0x06 => Marker
              case 0x07 => CuePoint
              case 0x08 => ProgramName
              case 0x09 => DeviceName
              case 0x20 => MidiChannelPrefix
              case 0x21 => MidiPort
              case 0x2F => EndOfTrack
              case 0x51 => Tempo
              case 0x54 => SmpteOffset
              case 0x58 => TimeSignature
              case 0x59 => KeySignature
              case 0x7F => SequencerSpecificEvent
            }
          } yield result
        )
      }

      val sysexEvent = {
        P(
          for{
            _ <- BS(0xF0).~/
            length <- vLength
            message <- AnyByte.rep(exactly = length).!
          } yield SysExEvent.Message(message)
        )
      }
      val trackEvent = P( midiEvent | metaEvent | sysexEvent  )

      val trackChunk: P[Seq[(Int, TrackEvent)]] = {
        P( hexBytes("4d 54 72 6b") ~ Int32 ~ (vTime ~ trackEvent).filter(_._2 != MetaEvent.EndOfTrack).rep() ~ (vTime ~ trackEvent) ).map{
          case (length, events, last) => events :+ last
        }
      }
      val midiParser: P[Midi] = P(
        for{
          (format, nTracks, tickDiv) <- header
          tracks <- trackChunk.rep(1) ~ End
        } yield Midi(format, tickDiv, tracks)
      )


      val parsed = midiParser.parse(lenaBytes)
      println(parsed.asInstanceOf[Parsed.Failure].extra.traced.trace)
      parsed
    }
  }
}
