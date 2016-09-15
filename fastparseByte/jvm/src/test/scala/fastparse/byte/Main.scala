package fastparse.byte

import javax.sound.midi.MidiSystem

/**
  * Exercise MidiParse by using it to parse and then play some music!
  *
  * - sbt "~fastparseJVM/test:run canon.mid"
  * - sbt "~fastparseJVM/test:run go.mid"
  * - sbt "~fastparseJVM/test:run ctend.mid"
  */
object Main {
  def main(args: Array[String]): Unit = {
    println("Parsing Midi...")
    val bytes = MidiTests.readResourceBytes("/" + args(0))

    val parsed = MidiParse.midiParser.parse(bytes).get.value
    println("format:\t"+parsed.format)
    println("tickDiv:\t"+parsed.tickDiv)
    play(parsed)
  }
  def play(midi: Midi) = {


    import Midi._
    import MidiData._
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
          case MetaEvent.Tempo(n) =>
            println(s"Tempo\t$n")
            currentTempo = n

          case MidiEvent(channelId, midiData) =>
            val channel = channels(channelId)
            midiData match{
              case NoteOn(note, velocity) => channel.noteOn(note, velocity)
              case NoteOff(note, velocity) => channel.noteOff(note, velocity)

              case ChannelPressure(p) =>
                println(s"Channel Pressure\t$p")
                channel.setChannelPressure(p)

              case PitchBend(p) =>
                println(s"Pitch Bend\t$p")
                channel.setPitchBend(p)

              case PolyphonicPressure(note, p) =>
                println(s"Pressure\t$note $p")
                channel.setPolyPressure(note, p)

              case Controller(c, v) =>
                println(s"Controller\t$c $v")
                channel.controlChange(c, v)

              case ProgramChange(p) =>
                 println(s"Program Change\t$p")
                 channel.programChange(p)
            }

          case _ => // do nothing
        }
        true
      }
    })()
  }
}
