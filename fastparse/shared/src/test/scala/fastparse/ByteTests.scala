package fastparse

import fastparse.Logger
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

        val Parsed.Failure(parser, 1, _) = ab.parse(strToBytes("01 01")) // or BS(1, 1)
        assert(parser == (BS(2): P0))
      }

      'repeat {
        import fastparse.byte._
        val ab = P(BS(1).rep ~ BS(2))
        val Parsed.Success(_, 8) = ab.parse(strToBytes("01 01 01 01 01 01 01 02"))
        val Parsed.Success(_, 4) = ab.parse(strToBytes("01 01 01 02"))

        val abc = P(BS(1).rep(sep = BS(2)) ~ BS(3))
        val Parsed.Success(_, 8) = abc.parse(strToBytes("01 02 01 02 01 02 01 03"))
        val Parsed.Failure(parser, 3, _) = abc.parse(strToBytes("01 02 01 01 02 01 03"))

        val ab4 = P(BS(1).rep(min = 2, max = 4, sep = BS(2)))
        val Parsed.Success(_, 7) = ab4.parse(strToBytes("01 02 01 02 01 02 01 02 01 02 01 02 01 02 01 02"))

        val ab4c = P(BS(1).rep(min = 2, max = 4, sep = BS(2)) ~ BS(3))
        val Parsed.Failure(_, 1, _) = ab4c.parse(strToBytes("01 03"))
        val Parsed.Success(_, 4) = ab4c.parse(strToBytes("01 02 01 03"))
        val Parsed.Success(_, 8) = ab4c.parse(strToBytes("01 02 01 02 01 02 01 03"))
        val Parsed.Failure(_, 7, _) = ab4c.parse(strToBytes("01 02 01 02 01 02 01 02 01 03"))
      }

      'option {
        import fastparse.byte._
        val option = P(BS(3).? ~ BS(1).rep(sep = BS(2)).! ~ End)

        val Parsed.Success(Array(1, 2, 1), 3) = option.parse(strToBytes("01 02 01"))
        val Parsed.Success(Array(1, 2, 1), 3) = option.parse(strToBytes("01 02 01"))
      }

      'either {
        import fastparse.byte._
        val either = P(BS(1).rep ~ (BS(2) | BS(3) | BS(4)) ~ End)

        val Parsed.Success(_, 6) = either.parse(strToBytes("01 01 01 01 01 02"))
        val Parsed.Failure(parser, 5, _) = either.parse(strToBytes("01 01 01 01 01 05"))
        assert(parser == (BS(2) | BS(3) | BS(4)))
      }


      'end {
        import fastparse.byte._
        val noEnd = P(BS(1).rep ~ BS(2))
        val withEnd = P(BS(1).rep ~ BS(2) ~ End)

        val Parsed.Success(_, 4) = noEnd.parse(strToBytes("01 01 01 02 01"))
        val Parsed.Failure(End, 4, _) = withEnd.parse(strToBytes("01 01 01 02 01"))

      }
      'start {
        import fastparse.byte._
        val ab = P(((BS(1) | Start) ~ BS(2)).rep ~ End).!

        val Parsed.Success(Array(1, 2, 1, 2), 4) = ab.parse(strToBytes("01 02 01 02"))
        val Parsed.Success(Array(2, 1, 2, 1, 2), 5) = ab.parse(strToBytes("02 01 02 01 02"))

        val Parsed.Failure(parser, 2, _) = ab.parse(strToBytes("01 02 02"))

      }

      'passfail {
        import fastparse.byte._
        val Parsed.Success((), 0) = Pass.parse(strToBytes("04 08 15 16 23 42"))
        val Parsed.Failure(Fail, 0, _) = Fail.parse(strToBytes("04 08 15 16 23 42"))
      }

      'index {
        import fastparse.byte._
        val finder = P(BS(1, 1, 1).rep ~ Index ~ BS(2, 2, 2) ~ BS(3, 3, 3).rep)

        val Parsed.Success(9, _) = finder.parse(strToBytes(" 01 01 01  01 01 01  01 01 01  02 02 02  03 03 03"))
      }

      'capturing {
        import fastparse.byte._
        val capture1 = P(BS(1).rep.! ~ BS(2) ~ End)

        val Parsed.Success(Array(1, 1, 1), 4) = capture1.parse(strToBytes("01 01 01 02"))

        val capture2 = P(BS(1).rep.! ~ BS(2).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2)), 4) = capture2.parse(strToBytes("01 01 01 02"))

        val capture3 = P(BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2), Array(3)), 5) = capture3.parse(strToBytes("01 01 01 02 03"))

        val captureRep = P(BS(1).!.rep ~ BS(2) ~ End)

        val Parsed.Success(Seq(Array(1), Array(1), Array(1)), 4) = captureRep.parse(strToBytes("01 01 01 02"))

        val captureOpt = P(BS(1).rep ~ BS(2).!.? ~ End)

        val Parsed.Success(Some(Array(2)), 4) = captureOpt.parse(strToBytes("01 01 01 02"))
      }
      'unapply {
        import fastparse.byte._
        val capture1 = P(BS(1).rep.! ~ BS(2) ~ End)

        val capture1(Array(1, 1, 1)) = strToBytes("01 01 01 02")

        val capture2 = P(BS(1).rep.! ~ BS(2).! ~ End)

        val capture2(Array(1, 1, 1), Array(2)) = strToBytes("01 01 01 02")

        val capture3 = P(BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End)

        val capture3(Array(1, 1, 1), Array(2), Array(3)) = strToBytes("01 01 01 02 03")

        val captureRep = P(BS(1).!.rep ~ BS(2) ~ End)

        val captureRep(Seq(Array(1), Array(1), Array(1))) = strToBytes("01 01 01 02")

        val captureOpt = P(BS(1).rep ~ BS(2).!.? ~ End)

        val captureOpt(Some(Array(2))) = strToBytes("01 01 01 02")
      }
      'anychar {
        import fastparse.byte._
        val ab = P(BS(1) ~ AnyByte.! ~ BS(1))

        val Parsed.Success(Array(0x42), 3) = ab.parse(strToBytes("01 42 01"))

        val Parsed.Failure(parser, 2, _) = ab.parse(strToBytes("01 42 43 01"))
        assert(parser == (BS(1): P0))
      }


      'lookahead {
        import fastparse.byte._
        val keyword = P((BS(1, 2, 3) ~ &(BS(4))).!.rep)

        val Parsed.Success(Seq(Array(1, 2, 3)), _) = keyword.parse(strToBytes("01 02 03 04"))
        val Parsed.Success(Seq(), __) = keyword.parse(strToBytes("01 02 03 05"))
      }
      'neglookahead {
        import fastparse.byte._
        val keyword = P(BS(1, 2, 3) ~ !BS(0) ~ AnyByte ~ BS(5, 6, 7)).!

        val Parsed.Success(Array(1, 2, 3, 0x42, 5, 6, 7), _) = keyword.parse(strToBytes("01 02 03 42 05 06 07"))

        val Parsed.Failure(parser, 4, _) = keyword.parse(strToBytes("01 02 03 00 05 06 07"))
        assert(parser == !BS(0))
      }
      'map {
        import fastparse.byte._
        val binary = P((BS(0) | BS(1)).rep.!)
        val binaryNum = P(binary.map(_.sum))

        val Parsed.Success(Array(1, 1, 0, 0), _) = binary.parse(strToBytes("01 01 00 00"))
        val Parsed.Success(2, _) = binaryNum.parse(strToBytes("01 01 00 00"))
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
          Foo.ints.parse(strToBytes("ff 00 00 00 00")),
          """Failure((int | longInt):0 ..."ff 00 00 00 00")"""
        )

      }
      'cuts{
        import fastparse.byte._
        object Foo{

          val int = P( BS(0) ~/ AnyByte.rep(exactly=4).! ).map(bytesToInt)
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(bytesToLongInt)
          val ints = P( (int | longInt).rep(1) )
        }
        check(
          Foo.ints.parse(strToBytes("ff 00 00 00 00")),
          """Failure(AnyElem:5 ..."")"""
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


        Foo.ints.parse(strToBytes("ff 00 00 00 00"))

        val expected = """
            +ints:0
              +int:0
              -int:0:Failure(int:0 / "00":0 ..."ff 00 00 00 00")
              +longInt:0
              -longInt:0:Failure(longInt:0 / AnyElem:5 ..."ff 00 00 00 00", cut)
            -ints:0:Failure(ints:0 / longInt:0 / AnyElem:5 ..."ff 00 00 00 00", cut)
                       """.lines.filter(_.trim != "").toSeq
        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
        val capturedString = captured.mkString("\n")

        assert(capturedString == expectedString)
      }
    }

  }
}
