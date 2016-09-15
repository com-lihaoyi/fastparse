package fastparse.byte
import java.nio.{ByteBuffer, ByteOrder}

import utest._

object ByteTests extends TestSuite {

  val tests = TestSuite {
    'basic {
      'simple {
        import fastparse.byte.all._

        val parseA = P( BS(1, 2, 3) )

        val Parsed.Success((), 3) = parseA.parse(Bytes(1, 2, 3))

        val Parsed.Success((), 3) = parseA.parse(hex"01 02 03")

        val Parsed.Failure(lastParser, index, extra) = parseA.parse(Bytes(2))
        assert(
          lastParser == (BS(1, 2, 3): P0),
          index == 0,
          extra.traced.trace == """parseA:0 / "01 02 03":0 ..."02""""
        )
      }

      'sequence {
        import fastparse.byte.all._

        val ab = P( BS(1) ~ BS(2) )

        val Parsed.Success(_, 2) = ab.parse(Bytes(1, 2))

        val Parsed.Failure(parser, 1, _) = ab.parse(hex"01 01") // or BS(1, 1)
        assert(parser == (BS(2): P0))
      }

      'repeat {
        import fastparse.byte.all._

        val ab = P( BS(1).rep ~ BS(2) )
        val Parsed.Success(_, 8) = ab.parse(hex"01 01 01 01 01 01 01 02")
        val Parsed.Success(_, 4) = ab.parse(hex"01 01 01 02")

        val abc = P( BS(1).rep(sep = BS(2)) ~ BS(3) )
        val Parsed.Success(_, 8) = abc.parse(hex"01 02 01 02 01 02 01 03")
        val Parsed.Failure(parser, 3, _) = abc.parse(hex"01 02 01 01 02 01 03")

        val ab4 = P( BS(1).rep(min = 2, max = 4, sep = BS(2)) )
        val Parsed.Success(_, 7) = ab4.parse(hex"01 02 01 02 01 02 01 02 01 02 01 02 01 02 01 02")

        val ab4c = P( BS(1).rep(min = 2, max = 4, sep = BS(2)) ~ BS(3) )
        val Parsed.Failure(_, 1, _) = ab4c.parse(hex"01 03")
        val Parsed.Success(_, 4) = ab4c.parse(hex"01 02 01 03")
        val Parsed.Success(_, 8) = ab4c.parse(hex"01 02 01 02 01 02 01 03")
        val Parsed.Failure(_, 7, _) = ab4c.parse(hex"01 02 01 02 01 02 01 02 01 03")
      }

      'optional {
        import fastparse.byte.all._

        val option = P( BS(3).? ~ BS(1).rep().! ~ End )

        val Parsed.Success(res1, 3) = option.parse(hex"03 01 01")
        assert(res1 == Bytes(1, 1))

        val Parsed.Success(res2, 2) = option.parse(hex"01 01")
        assert(res2 == Bytes(1, 1))

        val Parsed.Success(res3, 1) = option.parse(hex"01")
        assert(res3 == Bytes(1))
      }

      'either {
        import fastparse.byte.all._

        val either = P( (BS(2, 2) | BS(3, 3, 3) | BS(4)).rep() ~ End )

        // any combination of 04 or 02 02 or 03 03 03 succeeds
        val Parsed.Success(_, 6) = either.parse(hex"02 02 04 03 03 03")
        val Parsed.Success(_, 6) = either.parse(hex"02 02 04 02 02 04")

        // if there's a 01, which is none of the above options, fails
        val Parsed.Failure(parser, 3, _) = either.parse(hex"02 02 04 01")

      }


      'end {
        import fastparse.byte.all._

        val noEnd = P( BS(1).rep ~ BS(2) )
        val withEnd = P( BS(1).rep ~ BS(2) ~ End )

        val Parsed.Success(_, 4) = noEnd.parse(hex"01 01 01 02 01")
        val Parsed.Failure(End, 4, _) = withEnd.parse(hex"01 01 01 02 01")

      }

      'start {
        import fastparse.byte.all._

        val ab = P( ((BS(1) | Start) ~ BS(2)).rep ~ End ).!

        val Parsed.Success(res1, 4) = ab.parse(hex"01 02 01 02")
        assert(res1 == Bytes(1, 2, 1, 2))

        val Parsed.Success(res2, 5) = ab.parse(hex"02 01 02 01 02")
        assert(res2 == Bytes(2, 1, 2, 1, 2))

        val Parsed.Failure(parser, 2, _) = ab.parse(hex"01 02 02")

      }

      'passfail {
        import fastparse.byte.all._

        val Parsed.Success((), 0) = Pass.parse(hex"04 08 15 16 23 42")
        val Parsed.Failure(Fail, 0, _) = Fail.parse(hex"04 08 15 16 23 42")
      }

      'index {
        import fastparse.byte.all._

        val finder = P( BS(1, 1, 1).rep ~ Index ~ BS(2, 2, 2) ~ BS(3, 3, 3).rep )

        val Parsed.Success(9, _) = finder.parse(hex" 01 01 01  01 01 01  01 01 01  02 02 02  03 03 03")
      }

      'capture {
        import fastparse.byte.all._

        val capture1 = P( BS(1).rep.! ~ BS(2) ~ End )

        val Parsed.Success(res1, 4) = capture1.parse(hex"01 01 01 02")
        assert(res1 == Bytes(1, 1, 1))

        val capture2 = P( BS(1).rep.! ~ BS(2).! ~ End )

        val Parsed.Success(res2, 4) = capture2.parse(hex"01 01 01 02")
        assert(res2 == (Bytes(1, 1, 1), Bytes(2)))

        val capture3 = P( BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End )

        val Parsed.Success(res3, 5) = capture3.parse(hex"01 01 01 02 03")
        assert(res3 == (Bytes(1, 1, 1), Bytes(2), Bytes(3)))

        val captureRep = P( BS(1).!.rep ~ BS(2) ~ End )

        val Parsed.Success(res4, 4) = captureRep.parse(hex"01 01 01 02")
        assert(res4 == Seq(Bytes(1), Bytes(1), Bytes(1)))

        val captureOpt = P( BS(1).rep ~ BS(2).!.? ~ End )

        val Parsed.Success(res5, 4) = captureOpt.parse(hex"01 01 01 02")
        assert(res5 == Some(Bytes(2)))
      }

      'unapply {
        import fastparse.byte.all._

        val capture1 = P( BS(1).rep.! ~ BS(2) ~ End )

        val capture1(res1) = hex"01 01 01 02"
        assert(res1 == Bytes(1, 1, 1))

        val capture2 = P( BS(1).rep.! ~ BS(2).! ~ End )

        val capture2(res2_1, res2_2) = hex"01 01 01 02"
        assert(res2_1 == Bytes(1, 1, 1))
        assert(res2_2 == Bytes(2))

        val capture3 = P( BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End )

        val capture3(res3_1, res3_2, res3_3) = hex"01 01 01 02 03"
        assert(res3_1 == Bytes(1, 1, 1))
        assert(res3_2 == Bytes(2))
        assert(res3_3 == Bytes(3))

        val captureRep = P( BS(1).!.rep ~ BS(2) ~ End )

        val captureRep(res4) = hex"01 01 01 02"
        assert(res4 == Seq(Bytes(1), Bytes(1), Bytes(1)))

        val captureOpt = P( BS(1).rep ~ BS(2).!.? ~ End )

        val captureOpt(res5) = hex"01 01 01 02"
        assert(res5 == Some(Bytes(2)))
      }

      'anybyte {
        import fastparse.byte.all._

        val ab = P( BS(1) ~ AnyByte.! ~ BS(1) )

        val Parsed.Success(res, 3) = ab.parse(hex"01 42 01")
        assert(res == Bytes(0x42))

        val failure @ Parsed.Failure(parser, 2, _) = ab.parse(hex"01 42 43 01")

        assert(
          parser == (BS(1): P0),
          failure.msg == """ "01":2 ..."43 01" """.trim
        )

      }

      'lookahead {
        import fastparse.byte.all._

        val keyword = P( (BS(1, 2, 3) ~ &(BS(4))).!.rep )

        val Parsed.Success(res, _) = keyword.parse(hex"01 02 03 04")
        assert(res == Seq(Bytes(1, 2, 3))
        )
        val Parsed.Success(Seq(), __) = keyword.parse(hex"01 02 03 05")
      }

      'neglookahead {
        import fastparse.byte.all._

        val keyword = P( BS(1, 2, 3) ~ !BS(0) ~ AnyByte ~ BS(5, 6, 7) ).!

        val Parsed.Success(res, _) = keyword.parse(hex"01 02 03 42 05 06 07")
        assert(res == Bytes(1, 2, 3, 0x42, 5, 6, 7))

        val Parsed.Failure(parser, 4, _) = keyword.parse(hex"01 02 03 00 05 06 07")
        assert(parser == !BS(0))
      }

      'map {
        import fastparse.byte.all._

        val binary = P( (BS(0) | BS(1)).rep.! )

        val Parsed.Success(res, _) = binary.parse(hex"01 01 00 00")
        assert(res == Bytes(1, 1, 0, 0))

        val binaryNum = P( binary.map(_.toArray.sum) )
        val Parsed.Success(2, _) = binaryNum.parse(Bytes(0x01, 0x01, 0x00, 0x00))
      }

      'filter {
        import fastparse.byte.all._

        val nullTerminated = P( (Int8.filter(_ != 0).rep().! ~ BS(0)).rep() )

        val Parsed.Success(res, _) = nullTerminated .parse(
          hex"de ad be ef 00 13 37 00"
        )
        assert(res == Seq(hex"de ad be ef", hex"13 37"))
      }

      'flatMap {
        import fastparse.byte.all._

        val lengthPrefixed = P( Int8.flatMap(AnyBytes(_).!).rep() )

        val Parsed.Success(res, _) = lengthPrefixed.parse(
          Bytes(0x04, 0xde, 0xad, 0xbe, 0xef, 0x02, 0x13, 0x37)
        )
        assert(res == Seq(Bytes(0xde, 0xad, 0xbe, 0xef), Bytes(0x13, 0x37)))
      }


      'bytesWhile {
        import fastparse.byte.all._

        val nullTerminated = P( (BytesWhile(_ != 0).! ~ BS(0)).rep() )

        val Parsed.Success(res, _) = nullTerminated .parse(
          hex"de ad be ef 00 13 37 00"
        )
        assert(res == Seq(hex"de ad be ef", hex"13 37"))
      }

      'bytePred{
        import fastparse.byte.all._

        val nullTerminated = P( (BytePred(_ != 0).rep().! ~ BS(0)).rep() )

        val Parsed.Success(res, _) = nullTerminated .parse(
          Bytes(0xde, 0xad, 0xbe, 0xef, 0x00, 0x13, 0x37, 0x00)
        )
        assert(res == Seq(Bytes(0xde, 0xad, 0xbe, 0xef), Bytes(0x13, 0x37)))
      }
    }

    'debugging{
      import fastparse.byte.all._
      def check(a: Any, s: String) = assert(a.toString == s.trim)
      'original{
        object Foo{

          val int = P( BS(0) ~ AnyByte.rep(exactly=4).! ).map(_.toInt())
          val longInt = P( BS(-1) ~ AnyByte.rep(exactly=8).! ).map(_.toLong())
          val ints = P( (int | longInt).rep(1) )
        }

        check(
          Foo.ints.parse(hex"ff 00 00 00 00"),
          """Failure((int | longInt):0 ..."ff 00 00 00 00")"""
        )

      }
      'cuts{
        object Foo{

          val int = P( BS(0) ~/ AnyBytes(4).! ).map(_.toInt())
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(_.toLong())
          val ints = P( (int | longInt).rep(1) )
        }
        check(
          Foo.ints.parse(hex"ff 00 00 00 00"),
          """Failure(AnyByte:5 ..."")"""
        )
      }
      'log{
        val captured = collection.mutable.Buffer.empty[String]
        implicit val logger = fastparse.core.Logger(captured.append(_))
        object Foo{

          val int = P( BS(0) ~/ AnyByte.rep(exactly=4).! ).map(_.toInt()).log()
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(_.toLong()).log()
          val ints = P( (int | longInt).rep(1) ).log()
        }


        Foo.ints.parse(hex"ff 00 00 00 00")

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
      'bytevector{
        'construction{
          import fastparse.byte.all._

          // Constructing a short ByteVector from bytes
          val a = Bytes(0x01, 0xff)
          assert(a.toString == "ByteVector(2 bytes, 0x01ff)")

          // Constructing a ByteVector from an ASCII string
          val b  = Bytes("hello":_*)
          assert(b.toString == "ByteVector(5 bytes, 0x68656c6c6f)")

          // Constructing a ByteVector copying from a Array[Byte]
          val byteArray = Array[Byte](1, 2, 3, 4)
          val c = Bytes(byteArray)
          assert(c.toString == "ByteVector(4 bytes, 0x01020304)")

          // Unsafe construction from an Array[Byte], without copying; assumes
          // You do not mutate the underlying array, otherwise things break.
          val d = Bytes.view(byteArray)
          assert(d.toString == "ByteVector(4 bytes, 0x01020304)")

          // Hex Strings
          val e = hex"cafebabedeadbeef"
          assert(e.toString == "ByteVector(8 bytes, 0xcafebabedeadbeef)")
        }
        'operations{
          import fastparse.byte.all._

          val a = Bytes(0xff, 0xff)
          val b = Bytes(0x01, 0x01)

          // Simple operations
          assert(
            a.length == 2,
            a(0) == 0xff.toByte,
            b(0) == 0x01.toByte
          )

          // Concatenating byte vectors
          val c = a ++ b
          assert(c.toString == "ByteVector(4 bytes, 0xffff0101)")

          val d = b ++ a
          assert(d.toString == "ByteVector(4 bytes, 0x0101ffff)")

          // Converting to Arrays
          val arr = d.toArray
          assert(
            arr(0) == 1, arr(1) == 1,
            arr(2) == -1, arr(3) == -1
          )


          // Convenient conversions
          assert(
            c.toHex == "ffff0101",
            c.toBase64 == "//8BAQ==",
            Bytes.fromHex("ffff0101").get == c,
            Bytes.fromHex("""
              ff ff
              01 01
            """).get == c,
            Bytes.fromBase64("//8BAQ==").get == c
          )


        }
        'hexBytes{
          import fastparse.byte.all._

          // Constructing `Bytes` via their hex values
          assert(
            hex"01 ff" == Bytes(0x01, 0xff),

            hex"01 ff" == Bytes.fromHex("01 ff").get,

            hex"""
              de ad be ef
              ca fe ba be
            """ == Bytes(
              0xde, 0xad, 0xbe, 0xef,
              0xca, 0xfe, 0xba, 0xbe
            ),

            hex"""
            de ad be ef
            ca fe ba be
            """ == Bytes.fromHex("""
              de ad be ef
              ca fe ba be
            """).get
          )


          // You can interpolate Bytes into hex literals

          val beef = hex"de ad be ef"
          val cafeBytes = Bytes(0xca, 0xfe)
          assert(
            hex"$beef $cafeBytes ba be" == Bytes(
              0xde, 0xad, 0xbe, 0xef,
              0xca, 0xfe, 0xba, 0xbe
            )
          )

        }
      }
      'prettyBytesBare{
        import fastparse.byte.all._

        val blob = Bytes(
          "The quick brown fox jumps over the lazy dog. She sells " +
          "sea shells on the sea shore but the shells she sells she " +
          "sells no more. Peter piper picked a pack of pickled peppers.":_*
        )
        assert( prettyBytes(blob) ==
          """       0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0      54 68 65 20 71 75 69 63 6b 20 62 72 6f 77 6e 20
            |16     66 6f 78 20 6a 75 6d 70 73 20 6f 76 65 72 20 74
            |32     68 65 20 6c 61 7a 79 20 64 6f 67 2e 20 53 68 65
            |48     20 73 65 6c 6c 73 20 73 65 61 20 73 68 65 6c 6c
            |64     73 20 6f 6e 20 74 68 65 20 73 65 61 20 73 68 6f
            |80     72 65 20 62 75 74 20 74 68 65 20 73 68 65 6c 6c
            |96     73 20 73 68 65 20 73 65 6c 6c 73 20 73 68 65 20
            |112    73 65 6c 6c 73 20 6e 6f 20 6d 6f 72 65 2e 20 50
            |128    65 74 65 72 20 70 69 70 65 72 20 70 69 63 6b 65
            |       ...""".stripMargin
        )
      }
      'prettyBytesMarker{
        import fastparse.byte.all._

        val blob = Bytes(
          "The quick brown fox jumps over the lazy dog. She sells " +
          "sea shells on the sea shore but the shells she sells she " +
          "sells no more. Peter piper picked a pack of pickled peppers.":_*
        )

        assert( prettyBytes(blob, markers = Seq(75, 100)) ==
          """       0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0      54 68 65 20 71 75 69 63 6b 20 62 72 6f 77 6e 20
            |16     66 6f 78 20 6a 75 6d 70 73 20 6f 76 65 72 20 74
            |32     68 65 20 6c 61 7a 79 20 64 6f 67 2e 20 53 68 65
            |48     20 73 65 6c 6c 73 20 73 65 61 20 73 68 65 6c 6c
            |64     73 20 6f 6e 20 74 68 65 20 73 65 61 20 73 68 6f
            |                                        ^
            |80     72 65 20 62 75 74 20 74 68 65 20 73 68 65 6c 6c
            |96     73 20 73 68 65 20 73 65 6c 6c 73 20 73 68 65 20
            |                   ^
            |112    73 65 6c 6c 73 20 6e 6f 20 6d 6f 72 65 2e 20 50
            |128    65 74 65 72 20 70 69 70 65 72 20 70 69 63 6b 65
            |144    64 20 61 20 70 61 63 6b 20 6f 66 20 70 69 63 6b
            |160    6c 65 64 20 70 65 70 70 65 72 73 2e""".stripMargin
        )
      }
      'splash{
        import fastparse.byte.all._

        case class Struct(f: Float, i: Int, s: String, b: Bytes)

        val header = P( BS(0xDE, 0xAD, 0xBE, 0xEF) ~ BE.Float32 ~ BE.Int32 )

        val cString = P( BytesWhile(_ != 0).! ~ BS(0x0) ).map(x => new String(x.toArray))

        val varLengthBlob = BE.UInt16.flatMap{AnyBytes(_).!}

        val structThing = P( header ~ cString ~ varLengthBlob ~ End).map(Struct.tupled)

        val bytes = Bytes(
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
          new String(result.b.toArray) == "world"
        )

      }

      'bs{
        import fastparse.byte.all._

        val parser = P( BS(0xDE, 0xAD, 0xBE, 0xEF) )

        val Parsed.Success((), 4) = parser.parse(Bytes(0xDE, 0xAD, 0xBE, 0xEF))
        val Parsed.Failure(_, 0, _) = parser.parse(Bytes(0xCA, 0xFE, 0xBA, 0xBE))
      }

      'bsBytes{
        import fastparse.byte.all._

        val parser1 = P( BS(hex"deadbeef") )

        val Parsed.Success((), 4) = parser1.parse(Bytes(0xDE, 0xAD, 0xBE, 0xEF))
        val Parsed.Failure(_, 0, _) = parser1.parse(Bytes(0xCA, 0xFE, 0xBA, 0xBE))

        val bytes = Bytes(0xDE, 0xfAD, 0xBE, 0xEF)
        val parser2 = P( BS(bytes) )

        val Parsed.Success((), 4) = parser1.parse(Bytes(0xDE, 0xAD, 0xBE, 0xEF))
        val Parsed.Failure(_, 0, _) = parser1.parse(Bytes(0xCA, 0xFE, 0xBA, 0xBE))
      }

      'udp{
        import fastparse.byte.all._

        case class UdpPacket(sourcePort: Int,
                             destPort: Int,
                             checkSum: Int,
                             data: Bytes)

        // BE.UInt16 stands for big-endian unsigned-16-bit-integer parsers
        val udpHeader = P( BE.UInt16 ~ BE.UInt16 ~ BE.UInt16 ~ BE.UInt16 )

        val udpParser = P(
          for{
            (sourcePort, destPort, length, checkSum) <- udpHeader
            data <- AnyBytes(length - 8).!
          } yield UdpPacket(sourcePort, destPort, checkSum, data)
        )

        val bytes = hex"""
          04 89 00 35 00 2C AB B4 00 01 01 00 00 01 00 00
          00 00 00 00 04 70 6F 70 64 02 69 78 06 6E 65 74
          63 6F 6D 03 63 6F 6D 00 00 01 00 01
        """

        val Parsed.Success(packet, _) = udpParser.parse(bytes)
        assert(
          packet.sourcePort == 1161,
          packet.destPort == 53,
          packet.checkSum == 43956,
          packet.data.length == 36,
          packet.data == hex"""
            00 01 01 00 00 01 00 00 00 00 00 00 04 70 6F 70
            64 02 69 78 06 6E 65 74 63 6F 6D 03 63 6F 6D 00
            00 01 00 01
          """
        )
      }

      'words{
        import fastparse.byte.all._

        def allZeroesByteArray = Bytes(0, 0, 0, 0, 0, 0, 0, 0)

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
        import fastparse.byte.all._

        val p = P( Int8 )


        val Parsed.Success(result1, _) = p.parse(Bytes(123)) // 7b
        assert(result1 == 123)


        val Parsed.Success(result2, _) = p.parse(Bytes(-123)) // 85
        assert(result2 == -123)


        val Parsed.Success(result3, _) = p.parse(Bytes(-1)) // ff
        assert(result3 == -1)
      }


      'uint8{
        import fastparse.byte.all._

        val p = P( UInt8 )


        val Parsed.Success(result1, _) = p.parse(Bytes(123)) // 7b
        assert(result1 == 123)


        val Parsed.Success(result2, _) = p.parse(Bytes(-123)) // 85
        assert(result2 == 133)


        val Parsed.Success(result3, _) = p.parse(Bytes(-1)) // ff
        assert(result3 == 255)
      }

      'endian{
        import fastparse.byte.all._

        val p1 = P( BE.Int16 )
        val Parsed.Success(result1, _) = p1.parse(Bytes(1, 0)) // 01 00
        assert(result1 == 256)


        val p2 = P( LE.Int16 )
        val Parsed.Success(result2, _) = p2.parse(Bytes(1, 0)) // 01 00
        assert(result2 == 1)


        val p3 = P( BE.Int32 )
        val bytes3 = Bytes(-128, 0, 0, 0) // ff 00 00 00
        val Parsed.Success(result3, _) = p3.parse(bytes3)
        assert(result3 == -2147483648)


        val p4 = P( LE.Int32 )
        val bytes4 = Bytes(-128, 0, 0, 0) // ff 00 00 00
        val Parsed.Success(result4, _) = p4.parse(bytes4)
        assert(result4 == 128)
      }


      'unsignedEndian{
        import fastparse.byte.all._

        val p1 = P( BE.Int64 )
        val bytes1 = Bytes(1, 0, 0, 0, 0, 0, 0, 0) // 01 00 00 00 00 00 00 00
        val Parsed.Success(result2, _) = p1.parse(bytes1)
        assert(result2 == 72057594037927936L)


        val p2 = P( BE.Int32 )
        val bytes2 = Bytes(-128, -128, -128, -128) // 80 80 80 80
        val Parsed.Success(result3, _) = p2.parse(bytes2)
        assert(result3 == -2139062144)


        val p3 = P( BE.UInt32 )
        val bytes3 = Bytes(-128, -128, -128, -128) // 80 80 80 80
        val Parsed.Success(result4, _) = p3.parse(bytes3)
        assert(result4 == 2155905152L)
      }


      'floats{
        import fastparse.byte.all._

        val p1 = P( BE.Float32 )
        val bytes1 = Bytes(64, 73, 15, -37) // 40 49 0f db
        val Parsed.Success(result1, _) = p1.parse(bytes1)
        assert(math.abs(result1 - 3.1415927) < 0.00001)


        val p2 = P( LE.Float32 )
        val bytes2 = Bytes(-37, 15, 73, 64) // db 0f 49 40
        val Parsed.Success(result2, _) = p2.parse(bytes2)
        assert(math.abs(result2 - 3.1415927) < 0.00001)


        val p3 = P( BE.Float64 )
        val bytes3 = Bytes(64, 9, 33, -5, 84, 68, 45, 24) // 40 09 21 fb 54 44 2d 18
        val Parsed.Success(result3, _) = p3.parse(bytes3)
        assert(math.abs(result3 - 3.1415927) < 0.00001)
      }

    }

    'prettyBytes{
      import fastparse.byte.all._

      def check(inputHex: String,
                expected: String,
                markers: Seq[Int] = Seq(-1),
                contextRows: Int = 8) = {
        val pretty = fastparse.byte.all.prettyBytes(
          fastparse.byte.all.Bytes.fromHex(inputHex).get, markers, contextRows
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
          """     0  1  2  3
            |
            |0    00 01 01 00""".stripMargin
        )
      }
      'shortMarker{
        check(
          "00 01 01 00",
          """     0  1  2  3
            |
            |0    00 01 01 00
            |     ^""".stripMargin,
          markers = Seq(0)
        )
        check(
          "00 01 01 00",
          """     0  1  2  3
            |
            |0    00 01 01 00
            |     ^  ^  ^""".stripMargin,
          markers = Seq(0, 1, 2)
        )
        check(
          "00 01 01 00",
          """     0  1  2  3
            |
            |0    00 01 01 00
            |              ^""".stripMargin,
          markers = Seq(3)
        )
      }
      'truncate{
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |16      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin
        )
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |0       00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |16      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                                                     ^
            |144     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |160     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |176     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |192     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |208     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |224     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |240     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |256     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin,
          markers = Seq(143)
        )
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |        ...
            |16      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |32      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |48      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |64      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |80      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |96      00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |112     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |128     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |144     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ^
            |160     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |176     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |192     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |208     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |224     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |240     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |256     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |272     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin,
          markers = Seq(144)
        )
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |        ...
            |368     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |384     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |416     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |432     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |448     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |464     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |480     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                    ^
            |512     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |528     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |544     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |560     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |576     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |592     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |608     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |624     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin,
          markers = Seq(500)
        )

        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |        ...
            |272     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |288     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |304     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |320     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |336     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |352     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |368     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |384     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ^
            |416     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |432     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |448     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |464     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |480     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                    ^
            |512     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |528     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |544     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |560     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |576     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |592     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |608     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |624     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin,
          markers = Seq(400, 500)
        )
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |        ...
            |384     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |400     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ^
            |416     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...
            |480     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |496     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |                    ^
            |512     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |        ...""".stripMargin,
          markers = Seq(400, 500),
          contextRows = 1
        )
        check(
          "00 " * 1000,
          """        0  1  2  3  4  5  6  7  8  9  10 11 12 13 14 15
            |
            |        ...
            |864     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |880     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |896     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |912     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |928     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |944     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |960     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |976     00 00 00 00 00 00 00 00 00 00 00 00 00 00 00 00
            |992     00 00 00 00 00 00 00 00
            |                             ^""".stripMargin,
          markers = Seq(999)
        )
      }

    }

    'endianness{
      import fastparse.byte.all._
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
            val arr = Bytes.view(put(ByteBuffer.allocate(size).order(enum), i).array())
            val fastparse.byte.all.Parsed.Success(`i`, `size`) = parser.parse(arr)
          }

        }
        s"Checked $count different values from ${firstValue.get} to ${lastValue.get}"
      }
      def iterateShorts = (Short.MinValue to Short.MaxValue).toIterator
      'short - {
        check[Short](
          2,
          iterateShorts.map(_.toShort),
          fastparse.byte.all.BE.Int16,
          fastparse.byte.all.LE.Int16,
          (b, i) => b.putShort(i)
        )
      }
      'int{
        check[Int](
          4,
          iterateShorts.map(_ * Short.MaxValue),
          fastparse.byte.all.BE.Int32,
          fastparse.byte.all.LE.Int32,
          (b, i) => b.putInt(i)
        )
      }
      'long{
        check[Long](
          8,
          iterateShorts.map(_.toLong * Int.MaxValue * Short.MaxValue),
          fastparse.byte.all.BE.Int64,
          fastparse.byte.all.LE.Int64,
          (b, i) => b.putLong(i)
        )
      }
      'float{
        check[Float](
          4,
          iterateShorts.map(i => java.lang.Float.intBitsToFloat(i * Short.MaxValue))
                       .filterNot(java.lang.Float.isNaN),
          fastparse.byte.all.BE.Float32,
          fastparse.byte.all.LE.Float32,
          (b, i) => b.putFloat(i)
        )
      }
      'double{
        check[Double](
          8,
          iterateShorts.map(i => java.lang.Double.longBitsToDouble(i.toLong * Int.MaxValue * Short.MaxValue))
                       .filterNot(java.lang.Double.isNaN),
          fastparse.byte.all.BE.Float64,
          fastparse.byte.all.LE.Float64,
          (b, i) => b.putDouble(i)
        )
      }
    }
  }
}
