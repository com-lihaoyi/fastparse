package demo

import classparse.ClassParse
import cssparse.PrettyPrinter
import fastparse.byte.Midi
import org.scalajs.dom
import org.scalajs.dom.{Event, UIEvent, html}
import fastparse.utils.Utils

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typedarray.{ArrayBuffer, Uint8Array}

@JSExport
object DemoMain {
  @JSExport
  def midiparser(container: html.Div) = {
    import scalatags.JsDom.all._
    val uploadFile = input(
      width := "100%",
      id := "uploadFile",
      tpe := "file",
      "Upload Midi file"
    ).render
    val metadataBox = pre(
      display.flex, flex := 1, overflowX.scroll,
      "Upload a MIDI file and MidiParse will parse it, show you some metadata ",
      "extracted from it, and play it using HTML5 Audio.\n\nNote that when you ",
      "first upload a file, the browser takes time to generate the necessary sounds ",
      "and it may take a few seconds before the music starts."
    ).render
    val loggerBox = pre(display.flex, flex := 1, overflowX.scroll).render

    container.appendChild(uploadFile)
    container.appendChild(div(
      display.flex, flexDirection.row,
      height := 400, overflowY.scroll,
      metadataBox, loggerBox
    ).render)

    var notes = Vector.empty[String]
    var progress = ""

    def updateLogBox() = {
      loggerBox.textContent = (
        Seq(
          "--------Remaining Events--------",
          progress,
          "---------Current Notes----------"
        ) ++
        notes.take(1) ++
        Seq(
          "---------Earlier Notes----------"
        ) ++
        notes.slice(1, 10)
      ).mkString("\n")
    }

    uploadFile.onchange = (e: dom.Event) => {
      val reader = new dom.FileReader()
      reader.readAsArrayBuffer(uploadFile.files.item(0))
      reader.onload = (e: UIEvent) => {
        val array = new Uint8Array(reader.result.asInstanceOf[ArrayBuffer])
        val contents = fastparse.byte.all.Bytes.view(
          (for (i <- 0 until array.length) yield array.get(i).toByte).toArray
        )
        dom.console.log(contents.length)
        fastparse.byte.MidiParse.midiParser.parse(contents) match{
          case fastparse.byte.all.Parsed.Success(midi, index) =>
            metadataBox.textContent =
              s"""Successfully parsed ${index} bytes
                  |Midi Format: ${midi.format}
                  |Tick Division: ${midi.tickDiv}
                  |${midi.tracks.length} Tracks:
                  |${midi.tracks.zipWithIndex.map{case (t, i) => s"Track $i: ${t.length} events"}.mkString("\n")}
                 """.stripMargin
            playMusic(midi, {x => notes = x +: notes; updateLogBox()}, {x => progress = x; updateLogBox()})
          case fastparse.byte.all.Parsed.Failure(lastParser, index, extra) =>
            metadataBox.textContent = "Failed to parse midi\n" + extra.traced.trace
        }
      }
    }

  }
  val notes = Vector("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
  lazy val allNotes = for(octave <- 0 to 10; note <- notes) yield (note, octave)
  def makeNote(note: String, octave: Int) = {
    js.Dynamic.newInstance(js.Dynamic.global.Audio)(
      js.Dynamic.global.Synth.generate(0, note, octave - 2, 10)
    )
  }
  def playMusic(midi: fastparse.byte.Midi,
                noteLogger: String => Unit,
                progressLogger: String => Unit) = {
    import fastparse.byte.Midi._, MidiData._
    case class TrackInfo(var savedTicks: Int, var track: List[(Int, Midi.TrackEvent)]){
      def tillNext = track.head._1 - savedTicks
    }
    noteLogger("Initializing Musical Notes...")
    val notes =
      midi.tracks
          .flatten
          .collect{case (_, MidiEvent(channel, NoteOn(note, velocity))) => (channel, note)}
          .distinct
          .map{ case (channel, note) => ((channel, note), makeNote(allNotes(note)._1, allNotes(note)._2)) }
          .toMap

    val allTracks = midi.tracks.map(_.toList).map(TrackInfo(0, _))
    // 120bpm, in microseconds-per-quarter-beat
    var currentTempo = 1000000 * 120 / 60 / 4

    def tick(): Unit = {

      val logs = collection.mutable.Buffer.empty[String]
      progressLogger(allTracks.map(_.track.length).mkString(", "))
      while ({
        val remaining = allTracks.filter(_.track != Nil)
        val nextTrack = remaining.minBy(_.tillNext)
        val tillNext = nextTrack.tillNext

        if (tillNext == 0) {
          val nextEvent = nextTrack.track.head._2
          nextTrack.track = nextTrack.track.tail
          nextTrack.savedTicks = 0
          logs.append(nextEvent.toString)
          nextEvent match{
            case MetaEvent.Tempo(n) =>
              currentTempo = n

            case MidiEvent(channel, midiData) =>
              midiData match{
                case NoteOn(note, velocity) =>
                  notes((channel, note)).volume = velocity * 1.0 / 127
                  notes((channel, note)).play()
                case NoteOff(note, velocity) =>
                  notes((channel, note)).pause()
                  notes((channel, note)).currentTime = 0
                case _ => // skip the rest
              }

            case _ => // skip the rest
          }
          true
        } else if (remaining.nonEmpty){

          val milliSleep = midi.tickDiv match{
            case fastparse.byte.Midi.TickDiv.Metric(divisionsPerQuarterBeat) =>
              val milliTempo = currentTempo / 1000
              milliTempo * tillNext / divisionsPerQuarterBeat
            case fastparse.byte.Midi.TickDiv.TimeCode(fps, divs) =>
              ???
          }


          for(track <- remaining) track.savedTicks += tillNext
          assert(remaining.exists(_.tillNext == 0))
          logs.append("Sleeping:\t" + milliSleep + "ms")
          dom.setTimeout(() => tick(), milliSleep)
          false
        }else{
          // `remaining` is empty, nothing left to continue playing,
          // so just exit without setting a timeout
          false
        }
      })()
      noteLogger(logs.mkString("\n"))
    }
    tick()
  }
  @JSExport
  def scalaparser(container: html.Div) = {
    val example =
      """
        |package scalaparser
        |
        |/**
        | * Created by haoyi on 4/26/15.
        | */
        |object Main {
        |  def main(args: Array[String]): Unit = {
        |    println("Hello World!")
        |  }
        |}""".stripMargin
    helper(container, scalaparse.Scala.CompilationUnit, example)
  }
  @JSExport
  def math(container: html.Div) = {
    helper(container, fastparse.MathTests.expr, "((1+1*2)+(3*4*5))/3")
  }
  @JSExport
  def whitespaceMath(container: html.Div) = {
    helper(container, fastparse.WhiteSpaceMathTests.expr, "  (  (  1+1  * 2   ) +( 3* 4  *5  )  )/3")
  }
  @JSExport
  def indentation(container: html.Div) = {
    helper(
      container,
      fastparse.IndentationTests.expr,
      """+
        |  +
        |    1
        |    *
        |      1
        |      2
        |  *
        |    3
        |    4
        |    5""".stripMargin
    )
  }
  @JSExport
  def json(container: html.Div) = {
    helper(container, fastparse.JsonTests.jsonExpr,
      """{
        |  "firstName": "John",
        |  "lastName": "Smith",
        |  "age": 25,
        |  "address": {
        |      "streetAddress": "21 2nd Street",
        |      "city": "New York",
        |      "state": "NY",
        |      "postalCode": 10021
        |  },
        |  "phoneNumbers": [
        |      {
        |          "type": "home",
        |          "number": "212 555-1234"
        |      },
        |      {
        |          "type": "fax",
        |          "number": "646 555-4567"
        |      }
        |  ]
        |}""".stripMargin)
  }
  @JSExport
  def css(container: html.Div) = {
   import fastparse.all._
   helper(container, cssparse.CssRulesParser.ruleList.map(PrettyPrinter.printRuleList(_)),
   """b,
     |strong {
     |  font-weight:         bold;
     |}
     |dfn
     |{
     |  font-style:   italic;
     |}
     |h1 {
     |  margin: .67em 0;
     |
     |
     |  font-size: 2em
     |}""".stripMargin)
  }
  @JSExport
  def bmp(container: html.Div) = {
    import fastparse.byte.all._
    import fastparse.byte.BmpTests.BmpParse

    helperByteFile(container, BmpParse.bmp.map(bmp => {
      bmp.bitmapHeader match {
        case h: BmpParse.BmpAst.BitmapInfoHeader =>
          val p = h.infoPart
          s"""
            |Width: ${p.width}
            |Height: ${p.height}
            |Bit per pixel: ${p.bitsPerPixel}
            |Horizontal resolution ${p.horzRes}
            |Vertical resolution ${p.vertRes}
            |Image size ${p.imageSize}
          """.stripMargin
      }
    }))
  }
  @JSExport
  def clss(container: html.Div) = {
    import fastparse.byte.all._
    helperByteFile(container, classparse.ClassParse.classFile.map(c => {
      val ast = ClassParse.Ast.convertToAst(c)
      s"""
         |Fields:
         |${ast.fields.map(field => field.descriptor + " " + field.name).mkString("\n")}
         |
         |Methods:
         |${ast.methods.map(method => method.descriptor + " " + method.name).mkString("\n")}
       """.stripMargin
    }))
  }
  def helper(container: html.Div, parser: fastparse.all.Parser[_], default: String) = {
    import scalatags.JsDom.all._
    val inputBox = textarea(
      width := "45%",
      float.left,
      fontFamily := "monospace",
      fontSize := 16,
      default
    ).render

    val outputBox = div(width:="45%", float.right, overflowX.scroll).render

    def recalc() = {
      inputBox.rows = inputBox.value.lines.length
      val details = parser.parse(inputBox.value) match{
        case s: fastparse.all.Parsed.Success[_] =>
          table(
            width := "100%",
            tr(td("Success!")),
            tr(td("value:"), td(pre(s.value.toString)))
          )

        case fastparse.all.Parsed.Failure(lastParser, index, extra) =>
          val pretty = Utils.literalize( extra.input.slice( index, index + 15)).toString
          table(
            width := "100%",
            tr(td("Failure!")),
            tr(td("at index:"), td(code(index))),
            tr(td("found:"), td("...", code(pretty))),
            tr(td("expected:"), td(code(lastParser.toString)))
          )
      }
      outputBox.innerHTML = ""
      outputBox.appendChild(details.render)
    }
    recalc()
    inputBox.onkeyup = (e: dom.Event) => recalc()

    container.appendChild(div(inputBox, outputBox, div(clear.both)).render)
  }

  def helperByteFile(container: html.Div, parser: fastparse.byte.all.Parser[_]) = {
    import scalatags.JsDom.all._
    val uploadFile = input(
      width := "30%",
      float.left,
      id := "uploadFile",
      tpe := "file",
      "Upload file"
    ).render

    val outputBox = div(width:="65%", float.right, overflowX.scroll).render

    def recalc(e: dom.Event) = {
      val reader = new dom.FileReader()
      reader.readAsArrayBuffer(uploadFile.files.item(0))
      reader.onload = (e: UIEvent) => {
        val array = new Uint8Array(reader.result.asInstanceOf[ArrayBuffer])
        val contents = fastparse.byte.all.Bytes.view(
          (for (i <- 0 until array.length) yield array.get(i).toByte).toArray
        )

        val details = parser.parse(contents) match {
          case s: fastparse.byte.all.Parsed.Success[_] =>
            table(
              width := "100%",
              tr(td("Success!")),
              tr(td("value:"), td(pre(s.value.toString)))
            )

          case fastparse.byte.all.Parsed.Failure(lastParser, index, extra) =>
            table(
              width := "100%",
              tr(td("Failure!")),
              tr(td("at index:"), td(code(index))),
              tr(td("expected:"), td(code(lastParser.toString)))
            )
        }
        outputBox.innerHTML = ""
        outputBox.appendChild(details.render)
      }
    }

    uploadFile.onchange = (e: dom.Event) => recalc(e)
    container.appendChild(div(uploadFile, outputBox, div(clear.both)).render)
  }
}
