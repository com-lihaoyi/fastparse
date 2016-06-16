package fastparse

import utest._
import allByte._

object ByteTests extends TestSuite {

  val tests = TestSuite {
    'basic {
      'simple {
        val parseA = P(BS(1))

        val Parsed.Success(value, successIndex) = parseA.parse(BS(1))
        assert(value ==(), successIndex == 1)

        val failure = parseA.parse(BS(2)).asInstanceOf[Parsed.Failure]
        assert(
          failure.lastParser == (BS(1): P0),
          failure.index == 0,
          failure.extra.traced.trace == """parseA:1:1 / "01":1:1 ..."02""""
        )
      }

      'sequence {
        val ab = P(BS(1) ~ BS(2))

        val Parsed.Success(_, 2) = ab.parse("01 02".toBytes)

        val Parsed.Failure(parser, 1, _) = ab.parse("01 01".toBytes)
        assert(parser == (BS(2): P0))
      }

      'repeat {
        val ab = P(BS(1).rep ~ BS(2))
        val Parsed.Success(_, 8) = ab.parse("01 01 01 01 01 01 01 02".toBytes)
        val Parsed.Success(_, 4) = ab.parse("01 01 01 02".toBytes)

        val abc = P(BS(1).rep(sep = BS(2)) ~ BS(3))
        val Parsed.Success(_, 8) = abc.parse("01 02 01 02 01 02 01 03".toBytes)
        val Parsed.Failure(parser, 3, _) = abc.parse("01 02 01 01 02 01 03".toBytes)

        val ab4 = P(BS(1).rep(min = 2, max = 4, sep = BS(2)))
        val Parsed.Success(_, 7) = ab4.parse("01 02 01 02 01 02 01 02 01 02 01 02 01 02 01 02".toBytes)

        val ab4c = P(BS(1).rep(min = 2, max = 4, sep = BS(2)) ~ BS(3))
        val Parsed.Failure(_, 1, _) = ab4c.parse("01 03".toBytes)
        val Parsed.Success(_, 4) = ab4c.parse("01 02 01 03".toBytes)
        val Parsed.Success(_, 8) = ab4c.parse("01 02 01 02 01 02 01 03".toBytes)
        val Parsed.Failure(_, 7, _) = ab4c.parse("01 02 01 02 01 02 01 02 01 03".toBytes)
      }

      'option {
        val option = P(BS(3).? ~ BS(1).rep(sep = BS(2)).! ~ End)

        val Parsed.Success(Array(1, 2, 1), 3) = option.parse("01 02 01".toBytes)
        val Parsed.Success(Array(1, 2, 1), 3) = option.parse("01 02 01".toBytes)
      }

      'either {
        val either = P(BS(1).rep ~ (BS(2) | BS(3) | BS(4)) ~ End)

        val Parsed.Success(_, 6) = either.parse("01 01 01 01 01 02".toBytes)
        val Parsed.Failure(parser, 5, _) = either.parse("01 01 01 01 01 05".toBytes)
        assert(parser == (BS(2) | BS(3) | BS(4)))
      }


      'end {
        val noEnd = P(BS(1).rep ~ BS(2))
        val withEnd = P(BS(1).rep ~ BS(2) ~ End)

        val Parsed.Success(_, 4) = noEnd.parse("01 01 01 02 01".toBytes)
        val Parsed.Failure(End, 4, _) = withEnd.parse("01 01 01 02 01".toBytes)

      }
      'start {
        val ab = P(((BS(1) | Start) ~ BS(2)).rep ~ End).!

        val Parsed.Success(Array(1, 2, 1, 2), 4) = ab.parse("01 02 01 02".toBytes)
        val Parsed.Success(Array(2, 1, 2, 1, 2), 5) = ab.parse("02 01 02 01 02".toBytes)

        val Parsed.Failure(parser, 2, _) = ab.parse("01 02 02".toBytes)

      }

      'passfail {
        val Parsed.Success((), 0) = Pass.parse("04 08 15 16 23 42".toBytes)
        val Parsed.Failure(Fail, 0, _) = Fail.parse("04 08 15 16 23 42".toBytes)
      }

      'index {
        val finder = P(BS(1, 1, 1).rep ~ Index ~ BS(2, 2, 2) ~ BS(3, 3, 3).rep)

        val Parsed.Success(9, _) = finder.parse(" 01 01 01  01 01 01  01 01 01  02 02 02  03 03 03".toBytes)
      }

      'capturing {
        val capture1 = P(BS(1).rep.! ~ BS(2) ~ End)

        val Parsed.Success(Array(1, 1, 1), 4) = capture1.parse("01 01 01 02".toBytes)

        val capture2 = P(BS(1).rep.! ~ BS(2).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2)), 4) = capture2.parse("01 01 01 02".toBytes)

        val capture3 = P(BS(1).rep.! ~ BS(2).! ~ BS(3).! ~ End)

        val Parsed.Success((Array(1, 1, 1), Array(2), Array(3)), 5) = capture3.parse("01 01 01 02 03".toBytes)

        val captureRep = P(BS(1).!.rep ~ BS(2) ~ End)

        val Parsed.Success(Seq(Array(1), Array(1), Array(1)), 4) = captureRep.parse("01 01 01 02".toBytes)

        val captureOpt = P(BS(1).rep ~ BS(2).!.? ~ End)

        val Parsed.Success(Some(Array(2)), 4) = captureOpt.parse("01 01 01 02".toBytes)
      }
      'anychar {
        val ab = P(BS(1) ~ AnyByte.! ~ BS(1))

        val Parsed.Success(Array(0x42), 3) = ab.parse("01 42 01".toBytes)

        val Parsed.Failure(parser, 2, _) = ab.parse("01 42 43 01".toBytes)
        assert(parser == (BS(1): P0))
      }


      'lookahead {
        val keyword = P((BS(1, 2, 3) ~ &(BS(4))).!.rep)

        val Parsed.Success(Seq(Array(1, 2, 3)), _) = keyword.parse("01 02 03 04".toBytes)
        val Parsed.Success(Seq(), __) = keyword.parse("01 02 03 05".toBytes)
      }
      'neglookahead {
        val keyword = P(BS(1, 2, 3) ~ !BS(0) ~ AnyByte ~ BS(5, 6, 7)).!

        val Parsed.Success(Array(1, 2, 3, 0x42, 5, 6, 7), _) = keyword.parse("01 02 03 42 05 06 07".toBytes)

        val Parsed.Failure(parser, 4, _) = keyword.parse("01 02 03 00 05 06 07".toBytes)
        assert(parser == !BS(0))
      }
      'map {
        val binary = P((BS(0) | BS(1)).rep.!)
        val binaryNum = P(binary.map(_.sum))

        val Parsed.Success(Array(1, 1, 0, 0), _) = binary.parse("01 01 00 00".toBytes)
        val Parsed.Success(2, _) = binaryNum.parse("01 01 00 00".toBytes)
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
        object Foo{
          import fastparse.allByte._

          val int = P( BS(0) ~ AnyByte.rep(exactly=4).! ).map(bytesToInt)
          val longInt = P( BS(-1) ~ AnyByte.rep(exactly=8).! ).map(bytesToLongInt)
          val ints = P( (int | longInt).rep(1) )
        }

        check(
          Foo.ints.parse("ff 00 00 00 00".toBytes),
          """Failure((int | longInt):1:1 ..."ff 00 00 00 00")"""
        )

      }
      'cuts{
        object Foo{
          import fastparse.allByte._

          val int = P( BS(0) ~/ AnyByte.rep(exactly=4).! ).map(bytesToInt)
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(bytesToLongInt)
          val ints = P( (int | longInt).rep(1) )
        }
        check(
          Foo.ints.parse("ff 00 00 00 00".toBytes),
          """Failure(AnyElem():4:0 ..."")"""
        )
      }
      'log{
        val captured = collection.mutable.Buffer.empty[String]
        implicit val logger = Logger(captured.append(_))
        object Foo{
          import fastparse.allByte._

          val int = P( BS(0) ~/ AnyByte.rep(exactly=4).! ).map(bytesToInt).log()
          val longInt = P( BS(-1) ~/ AnyByte.rep(exactly=8).! ).map(bytesToLongInt).log()
          val ints = P( (int | longInt).rep(1) ).log()
        }


        Foo.ints.parse("ff 00 00 00 00".toBytes)

        val expected = """
            +ints:0
              +int:0
              -int:0:Failure(int:1:1 / "00":1:1 ..."ff 00 00 00 00")
              +longInt:0
              -longInt:0:Failure(longInt:1:1 / AnyElem():4:0 ..."ff 00 00 00 00", cut)
            -ints:0:Failure(ints:1:1 / longInt:1:1 / AnyElem():4:0 ..."ff 00 00 00 00", cut)
                       """.lines.filter(_.trim != "").toSeq
        val minIndent = expected.map(_.takeWhile(_ == ' ').length).min
        val expectedString = expected.map(_.drop(minIndent)).mkString("\n")
        val capturedString = captured.mkString("\n")

        assert(capturedString == expectedString)
      }
    }

  }
}
