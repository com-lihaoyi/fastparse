package fastparse

import java.nio.{ByteBuffer, ByteOrder}

import utest._

object ByteTests extends TestSuite {

  val tests = TestSuite {
    'basic {
      'simple {
        import fastparse.byte._
        val parseA = P(BS(1))

        val Parsed.Success(value, successIndex) = parseA.parse(BS(1))
        assert(value ==(), successIndex == 1)

        val failure = parseA.parse(BS(2)).asInstanceOf[Parsed.Failure]
        assert(
          failure.lastParser == (BS(1): P0),
          failure.index == 0,
          failure.extra.traced.trace == """parseA:0 / "01":0 ..."02""""
        )
      }

      'sequence {
        import fastparse.byte._
        val ab = P(BS(1) ~ BS(2)) // or P(Array[Byte](1) ~ Array[Byte](2))

        val Parsed.Success(_, 2) = ab.parse(BS(1, 2)) // BS(1, 2) == Array[Byte](1, 2)

        val Parsed.Failure(parser, 1, _) = ab.parse(hexBytes("01 01")) // or BS(1, 1)
        assert(parser == (BS(2): P0))
      }

      'repeat {
        import fastparse.byte._
        val ab = P(BS(1).rep ~ BS(2))
        val Parsed.Success(_, 8) = ab.parse(hexBytes("01 01 01 01 01 01 01 02"))
        val Parsed.Success(_, 4) = ab.parse(hexBytes("01 01 01 02"))

        val abc = P(BS(1).rep(sep = BS(2)) ~ BS(3))
        val Parsed.Success(_, 8) = abc.parse(hexBytes("01 02 01 02 01 02 01 03"))
        val Parsed.Failure(parser, 3, _) = abc.parse(hexBytes("01 02 01 01 02 01 03"))

        val ab4 = P(BS(1).rep(min = 2, max = 4, sep = BS(2)))
        val Parsed.Success(_, 7) = ab4.parse(hexBytes("01 02 01 02 01 02 01 02 01 02 01 02 01 02 01 02"))

        val ab4c = P(BS(1).rep(min = 2, max = 4, sep = BS(2)) ~ BS(3))
        val Parsed.Failure(_, 1, _) = ab4c.parse(hexBytes("01 03"))
        val Parsed.Success(_, 4) = ab4c.parse(hexBytes("01 02 01 03"))
        val Parsed.Success(_, 8) = ab4c.parse(hexBytes("01 02 01 02 01 02 01 03"))
        val Parsed.Failure(_, 7, _) = ab4c.parse(hexBytes("01 02 01 02 01 02 01 02 01 03"))
      }

      'option {
        import fastparse.byte._
        val option = P(BS(3).? ~ BS(1).rep(sep = BS(2)).! ~ End)

        val Parsed.Success(Array(1, 2, 1), 3) = option.parse(hexBytes("01 02 01"))
        val Parsed.Success(Array(1, 2, 1), 3) = option.parse(hexBytes("01 02 01"))
      }

      'either {
        import fastparse.byte._
        val either = P(BS(1).rep ~ (BS(2) | BS(3) | BS(4)) ~ End)

        val Parsed.Success(_, 6) = either.parse(hexBytes("01 01 01 01 01 02"))
        val Parsed.Failure(parser, 5, _) = either.parse(hexBytes("01 01 01 01 01 05"))
        assert(parser == (BS(2) | BS(3) | BS(4)))
      }


      'end {
        import fastparse.byte._
        val noEnd = P(BS(1).rep ~ BS(2))
        val withEnd = P(BS(1).rep ~ BS(2) ~ End)

        val Parsed.Success(_, 4) = noEnd.parse(hexBytes("01 01 01 02 01"))
        val Parsed.Failure(End, 4, _) = withEnd.parse(hexBytes("01 01 01 02 01"))

      }
      'start {
        import fastparse.byte._
        val ab = P(((BS(1) | Start) ~ BS(2)).rep ~ End).!

        val Parsed.Success(Array(1, 2, 1, 2), 4) = ab.parse(hexBytes("01 02 01 02"))
        val Parsed.Success(Array(2, 1, 2, 1, 2), 5) = ab.parse(hexBytes("02 01 02 01 02"))

        val Parsed.Failure(parser, 2, _) = ab.parse(hexBytes("01 02 02"))

      }

      'passfail {
        import fastparse.byte._
        val Parsed.Success((), 0) = Pass.parse(hexBytes("04 08 15 16 23 42"))
        val Parsed.Failure(Fail, 0, _) = Fail.parse(hexBytes("04 08 15 16 23 42"))
      }

      'index {
        import fastparse.byte._
        val finder = P(BS(1, 1, 1).rep ~ Index ~ BS(2, 2, 2) ~ BS(3, 3, 3).rep)

        val Parsed.Success(9, _) = finder.parse(hexBytes(" 01 01 01  01 01 01  01 01 01  02 02 02  03 03 03"))
      }

      'capturing {
        import fastparse.byte._
        val capture1 = P(BS(1).rep.! ~ BS(2) ~ End)

        val Parsed.Success(Array(1, 1, 1), 4) = capture1.parse(hexBytes("01 01 01 02"))

        val capture2 = P(BS(1).rep.! ~ BS(2).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2)), 4) = capture2.parse(hexBytes("01 01 01 02"))

        val capture3 = P(BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2), Array(3)), 5) = capture3.parse(hexBytes("01 01 01 02 03"))

        val captureRep = P(BS(1).!.rep ~ BS(2) ~ End)

        val Parsed.Success(Seq(Array(1), Array(1), Array(1)), 4) = captureRep.parse(hexBytes("01 01 01 02"))

        val captureOpt = P(BS(1).rep ~ BS(2).!.? ~ End)

        val Parsed.Success(Some(Array(2)), 4) = captureOpt.parse(hexBytes("01 01 01 02"))
      }
      'unapply {
        import fastparse.byte._
        val capture1 = P(BS(1).rep.! ~ BS(2) ~ End)

        val capture1(Array(1, 1, 1)) = hexBytes("01 01 01 02")

        val capture2 = P(BS(1).rep.! ~ BS(2).! ~ End)

        val capture2(Array(1, 1, 1), Array(2)) = hexBytes("01 01 01 02")

        val capture3 = P(BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End)

        val capture3(Array(1, 1, 1), Array(2), Array(3)) = hexBytes("01 01 01 02 03")

        val captureRep = P(BS(1).!.rep ~ BS(2) ~ End)

        val captureRep(Seq(Array(1), Array(1), Array(1))) = hexBytes("01 01 01 02")

        val captureOpt = P(BS(1).rep ~ BS(2).!.? ~ End)

        val captureOpt(Some(Array(2))) = hexBytes("01 01 01 02")
      }
      'anybyte {
        import fastparse.byte._
        val ab = P(BS(1) ~ AnyByte.! ~ BS(1))

        val Parsed.Success(Array(0x42), 3) = ab.parse(hexBytes("01 42 01"))

        val failure @ Parsed.Failure(parser, 2, _) = ab.parse(hexBytes("01 42 43 01"))

        assert(
          parser == (BS(1): P0),
          failure.msg == """ "01":2 ..."43 01" """.trim
        )

      }


      'lookahead {
        import fastparse.byte._
        val keyword = P((BS(1, 2, 3) ~ &(BS(4))).!.rep)

        val Parsed.Success(Seq(Array(1, 2, 3)), _) = keyword.parse(hexBytes("01 02 03 04"))
        val Parsed.Success(Seq(), __) = keyword.parse(hexBytes("01 02 03 05"))
      }
      'neglookahead {
        import fastparse.byte._
        val keyword = P(BS(1, 2, 3) ~ !BS(0) ~ AnyByte ~ BS(5, 6, 7)).!

        val Parsed.Success(Array(1, 2, 3, 0x42, 5, 6, 7), _) = keyword.parse(hexBytes("01 02 03 42 05 06 07"))

        val Parsed.Failure(parser, 4, _) = keyword.parse(hexBytes("01 02 03 00 05 06 07"))
        assert(parser == !BS(0))
      }
      'map {
        import fastparse.byte._
        val binary = P((BS(0) | BS(1)).rep.!)
        val binaryNum = P(binary.map(_.sum))

        val Parsed.Success(Array(1, 1, 0, 0), _) = binary.parse(hexBytes("01 01 00 00"))
        val Parsed.Success(2, _) = binaryNum.parse(hexBytes("01 01 00 00"))
      }
    }

    'debugging{
      def check(a: Any, s: String) = assert(a.toString == s.trim)

      def bytesToInt(bytes: Array[Byte]): Int = {
        var res = 0
        for (i <- bytes.indices) {
          res += bytes(i).toInt << ((3 - i) * 4)
        }
        res
      }

      def bytesToLongInt(bytes: Array[Byte]): Long = {
        var res: Long = 0
        for (i <- bytes.indices) {
          res += bytes(i).toLong << ((3 - i) * 4)
        }
        res
      }

      'original{
        import fastparse.byte._
        object Foo{

          val int = P( BS(0) ~ AnyByte.rep(exactly=4).! ).map(bytesToInt)
          val longInt = P( BS(-1) ~ AnyByte.rep(exactly=8).! ).map(bytesToLongInt)
          val ints = P( (int | longInt).rep(1) )
        }

        check(
          Foo.ints.parse(hexBytes("ff 00 00 00 00")),
          """Failure((int | longInt):0 ..."ff 00 00 00 00")"""
        )

      }
      'cuts{
        import fastparse.byte._
        object Foo{

          val int = P( BS(0) ~/ AnyBytes(4).! ).map(bytesToInt)
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(bytesToLongInt)
          val ints = P( (int | longInt).rep(1) )
        }
        check(
          Foo.ints.parse(hexBytes("ff 00 00 00 00")),
          """Failure(AnyByte:5 ..."")"""
        )
      }
      'log{
        import fastparse.byte._
        val captured = collection.mutable.Buffer.empty[String]
        implicit val logger = Logger(captured.append(_))
        object Foo{

          val int = P( BS(0) ~/ AnyByte.rep(exactly=4).! ).map(bytesToInt).log()
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(bytesToLongInt).log()
          val ints = P( (int | longInt).rep(1) ).log()
        }


        Foo.ints.parse(hexBytes("ff 00 00 00 00"))

        val expected = """
            +ints:0
              +int:0
              -int:0:Failure(int:0 / "00":0 ..."ff 00 00 00 00")
              +longInt:0
              -longInt:0:Failure(longInt:0 / AnyByte:5 ..."ff 00 00 00 00", cut)
            -ints:0:Failure(ints:0 / longInt:0 / AnyByte:5 ..."ff 00 00 00 00", cut)
                       """.lines.filter(_.trim != "").toSeq
        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
        val capturedString = captured.mkString("\n")

        assert(capturedString == expectedString)
      }
    }
    'example{
      'bs{
        import fastparse.byte._

        assert(BS(0x01, 0xff).toSeq == Array[Byte](0x01.toByte, 0xff.toByte).toSeq)
        assert(BS("hello":_*).toSeq == Array[Byte](104, 101, 108, 108, 111).toSeq)
      }
      'hexBytes{
        import fastparse.byte._

        assert(hexBytes("01 ff").toSeq == Array[Byte](0x01.toByte, 0xff.toByte).toSeq)
      }
      'prettyBytesBare{
        import fastparse.byte._
        val blob = BS(
          "The quick brown fox jumps over the lazy dog. She sells " +
          "sea shells on the sea shore but the shells she sells she " +
          "sells no more. Peter piper picked a pack of pickled peppers.":_*
        )
        assert( prettyBytes(blob) ==
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     54 68 65 20 71 75 69 63 6b 20 62 72 6f 77 6e 20
            |16    66 6f 78 20 6a 75 6d 70 73 20 6f 76 65 72 20 74
            |32    68 65 20 6c 61 7a 79 20 64 6f 67 2e 20 53 68 65
            |48    20 73 65 6c 6c 73 20 73 65 61 20 73 68 65 6c 6c
            |64    73 20 6f 6e 20 74 68 65 20 73 65 61 20 73 68 6f
            |80    72 65 20 62 75 74 20 74 68 65 20 73 68 65 6c 6c
            |96    73 20 73 68 65 20 73 65 6c 6c 73 20 73 68 65 20
            |112   73 65 6c 6c 73 20 6e 6f 20 6d 6f 72 65 2e 20 50
            |128   65 74 65 72 20 70 69 70 65 72 20 70 69 63 6b 65
            |      ...""".stripMargin
        )
      }
      'prettyBytesMarker{
        import fastparse.byte._
        val blob = BS(
          "The quick brown fox jumps over the lazy dog. She sells " +
          "sea shells on the sea shore but the shells she sells she " +
          "sells no more. Peter piper picked a pack of pickled peppers.":_*
        )

        assert( prettyBytes(blob, markers = Seq(75, 100)) ==
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     54 68 65 20 71 75 69 63 6b 20 62 72 6f 77 6e 20
            |16    66 6f 78 20 6a 75 6d 70 73 20 6f 76 65 72 20 74
            |32    68 65 20 6c 61 7a 79 20 64 6f 67 2e 20 53 68 65
            |48    20 73 65 6c 6c 73 20 73 65 61 20 73 68 65 6c 6c
            |64    73 20 6f 6e 20 74 68 65 20 73 65 61 20 73 68 6f
            |                                       ^
            |80    72 65 20 62 75 74 20 74 68 65 20 73 68 65 6c 6c
            |96    73 20 73 68 65 20 73 65 6c 6c 73 20 73 68 65 20
            |                  ^
            |112   73 65 6c 6c 73 20 6e 6f 20 6d 6f 72 65 2e 20 50
            |128   65 74 65 72 20 70 69 70 65 72 20 70 69 63 6b 65
            |144   64 20 61 20 70 61 63 6b 20 6f 66 20 70 69 63 6b
            |160   6c 65 64 20 70 65 70 70 65 72 73 2e""".stripMargin
        )
      }
      'splash{
        import fastparse.byte._

        case class Struct(f: Float, i: Int, s: String, b: Array[Byte])

        val header = P( BS(0xDE, 0xAD, 0xBE, 0xEF) ~ BE.Float32 ~ BE.Int32 )

        val cString = P( BytesWhile(_ != 0).! ~ BS(0x0) ).map(new String(_))

        val varLengthBlob = BE.UInt16.flatMap{AnyBytes(_).!}

        val structThing = P( header ~ cString ~ varLengthBlob ~ End).map(Struct.tupled)

        val bytes = BS(
          0xDE, 0xAD, 0xBE, 0xEF,
          64, 73, 15, -37,
          0, 0, 122, 105,
          104, 101, 108, 108, 111, 0,
          0, 5, 119, 111, 114, 108, 100
        )

        val Parsed.Success(result, _) = structThing.parse(bytes)

        
        assert(
          math.abs(result.f - Math.PI) < 0.0001,
          result.i == 31337,
          result.s == "hello",
          new String(result.b) == "world"
        )

      }

      'udp{
        import fastparse.byte._
        case class UdpPacket(sourcePort: Int,
                             destPort: Int,
                             checkSum: Int,
                             data: Array[Byte])

        // BE.UInt16 stands for big-endian unsigned-16-bit-integer parsers
        val udpHeader = P( BE.UInt16 ~ BE.UInt16 ~ BE.UInt16 ~ BE.UInt16 )

        val udpParser = P(
          for{
            (sourcePort, destPort, length, checkSum) <- udpHeader
            data <- AnyBytes(length - 8).!
          } yield UdpPacket(sourcePort, destPort, checkSum, data)
        )

        val bytes = hexBytes("""
          04 89 00 35 00 2C AB B4 00 01 01 00 00 01 00 00
          00 00 00 00 04 70 6F 70 64 02 69 78 06 6E 65 74
          63 6F 6D 03 63 6F 6D 00 00 01 00 01
        """)

        val Parsed.Success(packet, _) = udpParser.parse(bytes)
        assert(
          packet.sourcePort == 1161,
          packet.destPort == 53,
          packet.checkSum == 43956,
          packet.data.length == 36,
          packet.data.toSeq == hexBytes("""
            00 01 01 00 00 01 00 00 00 00 00 00 04 70 6F 70
            64 02 69 78 06 6E 65 74 63 6F 6D 03 63 6F 6D 00
            00 01 00 01
          """).toSeq
        )
      }

      'words{
        import fastparse.byte._

        def allZeroesByteArray = BS(0, 0, 0, 0, 0, 0, 0, 0)

        val p1 = P( AnyByte )
        val Parsed.Success((), index1) = p1.parse(allZeroesByteArray)
        assert(index1 == 1)


        val p2 = P( Word16 )
        val Parsed.Success((), index2) = p2.parse(allZeroesByteArray)
        assert(index2 == 2)


        val p3 = P( Word32 )
        val Parsed.Success((), index3) = p3.parse(allZeroesByteArray)
        assert(index3 == 4)


        val p4 = P( Word64 )
        val Parsed.Success((), index4) = p4.parse(allZeroesByteArray)
        assert(index4 == 8)
      }

      'int8{
        import fastparse.byte._

        val p = P( Int8 )


        val Parsed.Success(result1, _) = p.parse(BS(123)) // 7b
        assert(result1 == 123)


        val Parsed.Success(result2, _) = p.parse(BS(-123)) // 85
        assert(result2 == -123)


        val Parsed.Success(result3, _) = p.parse(BS(-1)) // ff
        assert(result3 == -1)
      }


      'uint8{
        import fastparse.byte._

        val p = P( UInt8 )


        val Parsed.Success(result1, _) = p.parse(BS(123)) // 7b
        assert(result1 == 123)


        val Parsed.Success(result2, _) = p.parse(BS(-123)) // 85
        assert(result2 == 133)


        val Parsed.Success(result3, _) = p.parse(BS(-1)) // ff
        assert(result3 == 255)
      }

      'endian{
        import fastparse.byte._

        val p1 = P( BE.Int16 )
        val Parsed.Success(result1, _) = p1.parse(BS(1, 0)) // 01 00
        assert(result1 == 256)


        val p2 = P( LE.Int16 )
        val Parsed.Success(result2, _) = p2.parse(BS(1, 0)) // 01 00
        assert(result2 == 1)


        val p3 = P( BE.Int32 )
        val bytes3 = BS(-128, 0, 0, 0) // ff 00 00 00
        val Parsed.Success(result3, _) = p3.parse(bytes3)
        assert(result3 == -2147483648)


        val p4 = P( LE.Int32 )
        val bytes4 = BS(-128, 0, 0, 0) // ff 00 00 00
        val Parsed.Success(result4, _) = p4.parse(bytes4)
        assert(result4 == 128)
      }


      'unsignedEndian{
        import fastparse.byte._


        val p1 = P( BE.Int64 )
        val bytes1 = BS(1, 0, 0, 0, 0, 0, 0, 0) // 01 00 00 00 00 00 00 00
        val Parsed.Success(result2, _) = p1.parse(bytes1)
        assert(result2 == 72057594037927936L)


        val p2 = P( BE.Int32 )
        val bytes2 = BS(-128, -128, -128, -128) // 80 80 80 80
        val Parsed.Success(result3, _) = p2.parse(bytes2)
        assert(result3 == -2139062144)


        val p3 = P( BE.UInt32 )
        val bytes3 = BS(-128, -128, -128, -128) // 80 80 80 80
        val Parsed.Success(result4, _) = p3.parse(bytes3)
        assert(result4 == 2155905152L)
      }


      'floats{
        import fastparse.byte._


        val p1 = P( BE.Float32 )
        val bytes1 = BS(64, 73, 15, -37) // 40 49 0f db
        val Parsed.Success(result1, _) = p1.parse(bytes1)
        assert(math.abs(result1 - 3.1415927) < 0.00001)


        val p2 = P( LE.Float32 )
        val bytes2 = BS(-37, 15, 73, 64) // db 0f 49 40
        val Parsed.Success(result2, _) = p2.parse(bytes2)
        assert(math.abs(result2 - 3.1415927) < 0.00001)


        val p3 = P( BE.Float64 )
        val bytes3 = BS(64, 9, 33, -5, 84, 68, 45, 24) // 40 09 21 fb 54 44 2d 18
        val Parsed.Success(result3, _) = p3.parse(bytes3)
        assert(math.abs(result3 - 3.1415927) < 0.00001)
      }

    }

    'prettyBytes{
      def check(inputHex: String,
                expected: String,
                markers: Seq[Int] = Seq(-1),
                contextRows: Int = 8) = {
        val pretty = fastparse.byte.prettyBytes(
          fastparse.byte.hexBytes(inputHex), markers, contextRows
        )
        assert(pretty == expected)
      }
      'multiline{
        check(
          """
          00 01 01 00 00 01 00 00 00 00 00 00 04 70 6F 70
          64 02 69 78 06 6E 65 74 63 6F 6D 03 63 6F 6D 00
          00 01 00 01
          """,

          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     00 01 01 00 00 01 00 00 00 00 00 00 04 70 6f 70
            |16    64 02 69 78 06 6e 65 74 63 6f 6d 03 63 6f 6d 00
            |32    00 01 00 01""".stripMargin
        )
      }
      'multilineExact{
        check(
          """
          00 01 01 00 00 01 00 00 00 00 00 00 04 70 6F 70
          64 02 69 78 06 6E 65 74 63 6F 6D 03 63 6F 6D 00
          """,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     00 01 01 00 00 01 00 00 00 00 00 00 04 70 6f 70
            |16    64 02 69 78 06 6e 65 74 63 6f 6d 03 63 6f 6d 00""".stripMargin
        )
      }
      'short{
        check(
          "00 01 01 00",
          """      0  1  2  3
            |
            |0     00 01 01 00""".stripMargin
        )
      }
      'shortMarker{
        check(
          "00 01 01 00",
          """      0  1  2  3
            |
            |0     00 01 01 00
            |      ^""".stripMargin,
          markers = Seq(0)
        )
        check(
          "00 01 01 00",
          """      0  1  2  3
            |
            |0     00 01 01 00
            |      ^  ^  ^""".stripMargin,
          markers = Seq(0, 1, 2)
        )
        check(
          "00 01 01 00",
          """      0  1  2  3
            |
            |0     00 01 01 00
            |               ^""".stripMargin,
          markers = Seq(3)
        )
      }
      'truncate{
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |16    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin
        )
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |16    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                                                   ^
            |144   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |160   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |176   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |192   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |208   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |224   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |240   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |256   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin,
          markers = Seq(143)
        )
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |      ...
            |16    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96    00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |144   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ^
            |160   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |176   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |192   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |208   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |224   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |240   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |256   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |272   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin,
          markers = Seq(144)
        )
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |      ...
            |368   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |384   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |416   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |432   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |448   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |464   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |480   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                  ^
            |512   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |528   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |544   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |560   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |576   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |592   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |608   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |624   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin,
          markers = Seq(500)
        )

        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |      ...
            |272   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |288   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |304   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |320   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |336   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |352   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |368   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |384   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ^
            |416   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |432   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |448   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |464   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |480   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                  ^
            |512   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |528   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |544   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |560   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |576   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |592   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |608   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |624   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin,
          markers = Seq(400, 500)
        )
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |      ...
            |384   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ^
            |416   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...
            |480   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                  ^
            |512   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |      ...""".stripMargin,
          markers = Seq(400, 500),
          contextRows = 1
        )
        check(
          "00 " * 1000,
          """      0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |      ...
            |864   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |880   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |896   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |912   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |928   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |944   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |960   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |976   00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |992   00 00 00 00 00 00 00 00
            |                           ^""".stripMargin,
          markers = Seq(999)
        )
      }

    }

    'endianness{
      import fastparse.byte._
      def check[T](size: Int,
                   range: Iterator[T],
                   beParser: P[T],
                   leParser: P[T],
                   put: (ByteBuffer, T) => ByteBuffer) = {
        var count = 0
        var firstValue: Option[T] = None
        var lastValue : Option[T] = None
        for (i <- range) {
          if(firstValue == None) firstValue = Some(i)
          lastValue = Some(i)
          count += 1
          val cases = Seq(
            ByteOrder.BIG_ENDIAN -> beParser,
            ByteOrder.LITTLE_ENDIAN -> leParser
          )
          for((enum, parser) <- cases){
            val arr = put(ByteBuffer.allocate(size).order(enum), i).array()
            val fastparse.byte.Parsed.Success(`i`, `size`) = parser.parse(arr)
          }

        }
        s"Checked $count different values from ${firstValue.get} to ${lastValue.get}"
      }
      def iterateShorts = (Short.MinValue to Short.MaxValue).toIterator
      'short - {
        check[Short](
          2,
          iterateShorts.map(_.toShort),
          fastparse.byte.BE.Int16,
          fastparse.byte.LE.Int16,
          (b, i) => b.putShort(i)
        )
      }
      'int{
        check[Int](
          4,
          iterateShorts.map(_ * Short.MaxValue),
          fastparse.byte.BE.Int32,
          fastparse.byte.LE.Int32,
          (b, i) => b.putInt(i)
        )
      }
      'long{
        check[Long](
          8,
          iterateShorts.map(_.toLong * Int.MaxValue * Short.MaxValue),
          fastparse.byte.BE.Int64,
          fastparse.byte.LE.Int64,
          (b, i) => b.putLong(i)
        )
      }
      'float{
        check[Float](
          4,
          iterateShorts.map(i => java.lang.Float.intBitsToFloat(i * Short.MaxValue))
                       .filterNot(java.lang.Float.isNaN),
          fastparse.byte.BE.Float32,
          fastparse.byte.LE.Float32,
          (b, i) => b.putFloat(i)
        )
      }
      'double{
        check[Double](
          8,
          iterateShorts.map(i => java.lang.Double.longBitsToDouble(i.toLong * Int.MaxValue * Short.MaxValue))
                       .filterNot(java.lang.Double.isNaN),
          fastparse.byte.BE.Float64,
          fastparse.byte.LE.Float64,
          (b, i) => b.putDouble(i)
        )
      }
    }
  }
}
