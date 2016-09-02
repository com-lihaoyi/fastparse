package fastparse

import javax.sound.midi.MidiSystem

/**
  * Created by haoyi on 9/2/16.
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("Parsing Midi...")
    val bytes = MidiTests.readResourceBytes("/" + args(0) + ".mid")
    MidiTests.hexBytes(bytes)
    val parsed = MidiParser.midiParser.parse(bytes).get.value
    println("format:\t"+parsed.format)
    println("tickDiv:\t"+parsed.tickDiv)
    play(parsed)
  }
  def play(midi: Midi) = {


    import Midi._, MidiData._
    // 120bpm, in microseconds-per-quarter-beat
    var currentTempo = 1000000 * 120 / 60 / 4
    println("defaultTempo:\t"+currentTempo)
//    midi.tracks(0).collect{ case Midi.MetaEvent.Tempo(n) => n}

    val synth = MidiSystem.getSynthesizer()
    val channels = synth.getChannels()
    synth.open()
    case class TrackInfo(var savedTicks: Int, var track: List[(Int, Midi.TrackEvent)]){
      def tillNext = track.head._1 - savedTicks
    }
    val allTracks = midi.tracks.map(_.toList).map(TrackInfo(0, _))

    println("playing...")
    while({
      val remaining = allTracks.filter(_.track != Nil)
//      println("remaining")
//      println(allTracks.map(_.track.length))
      if (remaining.isEmpty) false
      else{
        val nextTrack = remaining.minBy(_.tillNext)
        if (nextTrack.tillNext > 0) {
          val tillNext = nextTrack.tillNext
          val milliSleep = midi.tickDiv match{
            case TickDiv.Metric(divisionsPerQuarterBeat) =>
              val milliTempo = currentTempo / 1000
              milliTempo * nextTrack.tillNext / divisionsPerQuarterBeat
            case TickDiv.TimeCode(fps, divs) =>
              ???
          }
          Thread.sleep(milliSleep)
          for(track <- allTracks) track.savedTicks += tillNext
        }

        nextTrack.savedTicks = 0
        val nextEvent = nextTrack.track.head._2
        nextTrack.track = nextTrack.track.tail
        nextEvent match{
          case MetaEvent.Tempo(n) => currentTempo = n

          case MidiEvent(channelId, midiData) =>
            val channel = channels(channelId)
            midiData match{
              case NoteOn(note, velocity) => channel.noteOn(note, velocity)
              case NoteOff(note, velocity) => channel.noteOff(note, velocity)
              case ChannelPressure(p) => channel.setChannelPressure(p)
              case PitchBend(p) => channel.setPitchBend(p)
              case PolyphonicPressure(note, p) => channel.setPolyPressure(note, p)
              case Controller(c, v) => channel.controlChange(c, v)
              case ProgramChange(p) => channel.programChange(p)
            }

          case _ => // do nothing
        }
        true
      }
    })()
  }
}
