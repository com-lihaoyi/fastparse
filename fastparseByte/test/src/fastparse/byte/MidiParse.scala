package fastparse.byte
import fastparse.byte.all._
import BE._
case class Midi(format: Int, tickDiv: Midi.TickDiv, tracks: Seq[Seq[(Int, Midi.TrackEvent)]])
object Midi{
  sealed trait TickDiv
  object TickDiv {
    case class Metric(divisionsPerQuarterBeat: Short) extends TickDiv
    case class TimeCode(framesPerSecond: Int, subFrameDivisions: Int) extends TickDiv
  }
  sealed trait TrackEvent
  case class MidiEvent(channel: Byte, data: MidiData) extends TrackEvent
  sealed trait MidiData
  object MidiData{
    case class NoteOff(note: Byte, velocity: Byte) extends MidiData
    case class NoteOn(note: Byte, velocity: Byte) extends MidiData
    case class PolyphonicPressure(note: Byte, pressure: Byte) extends MidiData
    case class Controller(controller: Byte, value: Byte) extends MidiData
    case class ProgramChange(program: Byte) extends MidiData
    case class ChannelPressure(pressure: Byte) extends MidiData
    case class PitchBend(pitch: Short) extends MidiData
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
    case class Tempo(microSecondsPerQuarterBeat: Int) extends MetaEvent
    case class SmpteOffset(hour: Byte, minute: Byte, seconds: Byte, frames: Byte, fractional: Byte) extends MetaEvent
    case class TimeSignature(numerator: Byte, denominator: Byte, clocks: Byte, notatedNotes: Byte) extends MetaEvent
    case class KeySignature(sf: Byte, majorKey: Boolean) extends MetaEvent
    case class SequencerSpecificEvent(data: Bytes) extends MetaEvent
    case class Unknown(data: Bytes) extends MetaEvent
  }
  sealed trait SysExEvent extends TrackEvent
  object SysExEvent{
    case class Message(data: Bytes) extends SysExEvent
  }
}


/**
  * Built based on the definitions:
  *
  * http://www.ccarh.org/courses/253/handout/smf/
  * http://www.somascape.org/midi/tech/mfile.html#midi
  */
object MidiParse{

  import Midi._
  val midiHeader = P( BS(hex"4d 54 68 64 00 00 00 06") ~ Int16 ~ Int16 ~ Int16 )
  val deltaTime = P( BytesWhile(_ < 0, min = 0) ~ AnyByte )

  /**
    * Represents a variable length integer, in the midi file format. If a byte
    * starts with a 1 in it's high-order-bit, it is a "continuation" byte and
    * the `varInt` continues, until it reaches a byte with a 0 in it's
    * high-order-bit. To get the value out, remove every high-order bit and
    * concat all the 7-bit numbers into one entire, unsigned integer
    */
  val varInt: P[Int] = P( BytesWhile(b => (b & 0x80) != 0, min = 0) ~ Int8 ).!.map{ r =>
    r.toArray.map(_ & 0xff).foldLeft(0)((a, b) => (a << 7) + (b & ~0x80))
  }
  // Variable-length str/byte-array parsers, parsing {length + contents}
  val varBytes = varInt.flatMap(x => AnyBytes(x).!)

  val varString = varBytes.map(x => new String(x.toArray))


  val midiEvent: P[(MidiEvent, Seq[(Int, MidiEvent)])] = {
    val posInt8 = Int8.filter(_>=0)
    val NoteOff = (posInt8 ~ posInt8).map(MidiData.NoteOff.tupled)
    val NoteOn = (posInt8 ~ posInt8).map{
      // A default value of 64 is used in the absence of velocity sensors.
      // A value of 0 has a special meaning and is interpreted as a Note Off
      // (thus allowing the use of running status for a sequence of Note On and Off commands).
      case (note, 0) => MidiData.NoteOff(note, 64)
      case (note, velocity) => MidiData.NoteOn(note, velocity)
    }
    val PolyphonicPressure = (posInt8 ~ posInt8).map(MidiData.PolyphonicPressure.tupled)
    val Controller = (posInt8 ~ posInt8).map(MidiData.Controller.tupled)
    val ProgramChange = posInt8.map(MidiData.ProgramChange)
    val ChannelPressure = posInt8.map(MidiData.ChannelPressure)
    val PitchBend = (posInt8 ~ posInt8).map{case (a, b) => MidiData.PitchBend(((a >> 7) + b).toShort)}
    val statusByte: P[Byte] = P( ByteIn((0x80 to 0xef).map(_.toByte)).!.map(_(0)) )
    P(
      for{
        byte <- statusByte
        dataParser = byte & 0xf0 match{
          case 0x80 => NoteOff
          case 0x90 => NoteOn
          case 0xA0 => PolyphonicPressure
          case 0xB0 => Controller
          case 0xC0 => ProgramChange
          case 0xD0 => ChannelPressure
          case 0xE0 => PitchBend
        }
        parser = dataParser.map(MidiEvent((byte & 0x0f).toByte, _))
        // Allow repetitions of `parser` without needing to re-parse the
        // `statusByte` to support Midi "running status" syntax
        result <- parser ~ (varInt ~ parser).rep()
      } yield result
    )
  }
  val EndOfTrack = BS(0x00).map(_ => MetaEvent.EndOfTrack)
  val metaEvent = {

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

    val Tempo = (BS(0x03) ~ UInt8 ~ UInt8 ~ UInt8).map{case (a, b, c) => MetaEvent.Tempo((a << 16) + (b << 8) + c) }
    val SmpteOffset = (BS(0x05) ~ Int8 ~ Int8 ~ Int8 ~ Int8 ~ Int8).map(MetaEvent.SmpteOffset.tupled)
    val TimeSignature = (BS(0x04) ~ Int8 ~ Int8 ~ Int8 ~ Int8).map(MetaEvent.TimeSignature.tupled)
    val KeySignature = (BS(0x02) ~ Int8 ~ Int8).map{case (x, y) => MetaEvent.KeySignature(x, y != 0)}
    val SequencerSpecificEvent = varBytes.map(MetaEvent.SequencerSpecificEvent)
    val Unknown = varBytes.map(MetaEvent.Unknown)

    P(
      for{
        tpe <- Int8
        result <- tpe match {
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
          // Not sure why this is necessary: I'm seeing `0x10` flags turning up
          // in some of my MIDIs but I can't find any reference to them online
          case _ => Unknown
        }
      } yield result
    )
  }

  val sysexEvent = P( varBytes.map(SysExEvent.Message) )

  val trackEvent: P[(TrackEvent, Seq[(Int, TrackEvent)])] = {
    P( BS(0xFF)  ~/ metaEvent.map(_ -> Nil) | BS(0xF0) ~/ sysexEvent.map(_ -> Nil) | midiEvent )
  }

  val trackItemEnd = P( varInt ~ BS(0xFF, 0x2F)  ~/ EndOfTrack )
  val negTrackItemEnd = P( !trackItemEnd )
  val trackItem = P( negTrackItemEnd ~ varInt ~ trackEvent ).map{
    case (time, (event, rest)) => (time, event) +: rest
  }
  val trackHeader = P( BS(hex"4d 54 72 6b") ~/ Int32 )
  val trackChunk: P[Seq[(Int, TrackEvent)]] = {
    P( trackHeader ~ trackItem.rep() ~ trackItemEnd ).map{
      case (length, events, last) => events.flatten :+ last
    }
  }

  val midiParser: P[Midi] = P(
    for{
      (format, nTracks, rawTickDiv) <- midiHeader
      tracks <- trackChunk.rep(1) ~ End
    } yield {
      assert(nTracks == tracks.length)
      val tickDiv =
        if((rawTickDiv & 0x8000) == 0) TickDiv.Metric((rawTickDiv & ~0x80).toShort)
        else TickDiv.TimeCode(-(rawTickDiv >> 8).toByte, (rawTickDiv & 0xff).toShort)

      Midi(format, tickDiv, tracks)
    }
  )
}