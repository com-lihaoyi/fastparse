package fastparse

import fastparse.core.{ Parsed, ParseError }
import fastparse.core.Logger
import utest._
import scala.annotation.tailrec

/**
 *
 */
object TokenTests extends TestSuite {

  val tests = TestSuite {
    sealed abstract trait Part
    sealed abstract trait OpPart extends Part
    case object Plus extends OpPart
    case object Minus extends OpPart
    case class VarPart(factor: Int, exponent: Int) extends Part
        with Result {
      override def toString = exponent match {
        case 0 => s"$factor"
        case 1 => s"${factor}x"
        case e => s"${factor}x^$e"
      }
    }

    object ArithmeticParser {
      import fastparse.all._

      val minus = P("-".?.!.map(x => if (x.nonEmpty) -1 else 1))

      val digits = P(CharIn('0' to '9').rep(1).!.map(c => c.toInt))

      val number: P[Int] = P((minus ~ digits).map(a => a._1 * a._2))

      val name = P("x")

      val exponent: P[Int] = P((("^" ~ number) |
        "\u00B2".!.map(_ => 2) |
        "\u00B3".!.map(_ => 3) |
        "\u00B9".!.map(_ => 1)).?.map(_.getOrElse(1)))

      val operator: P[OpPart] = P("+".!.map(_ => Plus) | "-".!.map(_ => Minus))

      /**
       * x² => 2
       *  x => 1
       * "" => 0
       */
      val exponentOpt: P[Int] = P((name ~ exponent).?.map(_.getOrElse(0)))

      /**
       * 8x² => VarPart( 8, 2)
       *  5x => VarPart( 5, 1)
       *  70 => VarPart(70, 0)
       */
      val part: P[VarPart] = P((number ~ exponentOpt).map({ a => VarPart(a._1, a._2) }))

      val elements: P[Seq[Part]] = P((part ~ (operator ~ part).rep(0)).map({
        case (head, tail) =>
          ((Plus, head) +: tail)
            .map({
              case (Minus, v) => v.copy(factor = -v.factor)
              case (Plus, v)  => v
            })
            .sortBy(_.factor)
            .sortBy(_.exponent)
            .reverse
      }))

      val complete = P(elements ~ End)
      def apply(s: String) = complete.parse(s, 0)
    }

    'testArithmeticParse{
      def check(src: String, res: Part*) = {
        val fastparse.all.Parsed.Success(value, _) = ArithmeticParser(src)
        assert(value == res); value.mkString(" + ")
      }

      "64x²-4" - { check("64x^2-4", VarPart(64, 2), VarPart(-4, 0)) }
      "-4+64x²" - { check("-4+64x^2", VarPart(64, 2), VarPart(-4, 0)) }
      "8x+2x^2+16" - { check("8x+2x^2+16", VarPart(2, 2), VarPart(8, 1), VarPart(16, 0)) }
      "8x+-2x^2+16" - { check("8x+-2x^2+16", VarPart(-2, 2), VarPart(8, 1), VarPart(16, 0)) }
      "8x-2x^2+16" - { check("8x-2x^2+16", VarPart(-2, 2), VarPart(8, 1), VarPart(16, 0)) }
      "8x-2x^2-16" - { check("8x-2x^2-16", VarPart(-2, 2), VarPart(8, 1), VarPart(-16, 0)) }
    }

    object allToken extends fastparse.sequence.SeqApi[Part]

    object FactorizerParser {
      import allToken._

      def namedSquare = F({ case v @ VarPart(f, 2) => f }) // f*x**2 form
      def named = F({ case v @ VarPart(f, 1) => f }) // f*x form (=f*x**1)
      def value = F({ case v @ VarPart(f, 0) => f }) // f form (=f*x**0)
      def any = F({ case v: VarPart => v }) // any input

      /**
       * (ax)**2 + 2abx + (b)**2 => (ax+b)**2
       * (ax)**2 - 2abx + (b)**2 => (ax-b)**2
       * (ax)** - (b)**2        => (ax+b)(ax-b)
       */
      val factorize = P((namedSquare ~ named.? ~ value)
        .flatMap({
          case (sqa, dblabOpt, sqb) =>
            val a = Math.pow(sqa.abs, 0.5).toInt
            val b = Math.pow(sqb.abs, 0.5).toInt
            dblabOpt match {
              case Some(dblab) => // We've 3 components
                if (sqa > 0 && sqb > 0 && a * b * 2 == dblab.abs) {
                  val mhs = Seq(VarPart(a, 1), VarPart(b, 0)) // ax+b
                  val r =
                    if (dblab < 0) Diff(mhs: _*) // -2abx, ok returns (ax-b)
                    else Sum(mhs: _*) // +2abx, ok returns (ax+b)
                  PassWith(Square(r)) // Wrap in Square the previous result
                } else {
                  Fail
                }
              case None if sqb >= 0 => // We've 2 components but we need -b**2, not +b**2
                Fail
              case None => // We've 2 components, ok returns (ax+b)(ax-b)
                PassWith(Product(
                    Sum(VarPart(a, 1), VarPart(b, 0)), 
                    Diff(VarPart(a, 1), VarPart(b, 0))))
            }
        }))

      /**
       * Try to factorize, else returns initials values wrapped in a NonFactorizable
       */
      val complete = P((factorize | any.rep(1).map(NotFactorizable(_: _*))) ~ End)

      def apply(input: Seq[Part]) = complete.parse(input)
    }

    implicit class RichParsed[A, B](p: fastparse.core.Parsed[A, _, _]) {
      import fastparse.core._
      def withOtherParse(pf: A => Parsed[B, _, _]) = p match {
        case Parsed.Success(parsed, _) => pf(parsed)
        case f: Parsed.Failure[_, _]   => f
      }
    }

    sealed abstract trait Result { self =>
      override def toString = self match {
        case Square(value)                => value + "^2"
        case Product(value)               => s"$value"
        case Product(values @ _*)         => values.mkString(" * ")
        case Sum(value)                   => s"$value"
        case Sum(values @ _*)             => values.mkString("(", " + ", ")")
        case Diff(value)                  => s"$value"
        case Diff(values @ _*)            => values.mkString("(", " - ", ")")
        case NotFactorizable(values @ _*) => values.mkString("not factorizable : ", " + ", "")
      }
    }
    case class Square(value: Result) extends Result
    case class Product(values: Result*) extends Result
    case class Sum(values: Part*) extends Result
    case class Diff(values: Part*) extends Result
    case class NotFactorizable(values: Part*) extends Result

    'testReducerParser{
      def check(src: String, res: Any) = {
        ArithmeticParser(src) withOtherParse { parsed =>
          FactorizerParser(parsed)
        } match {
          case Parsed.Success(value, _) =>
            assert(value == res); value.toString
          case f: Parsed.Failure[_, _] => throw ParseError(f)
        }
      }

      "4x²+32x+64" - { check("32x+4x²+64", Square(Sum(VarPart(2, 1), VarPart(8, 0)))) }
      "64x²-32x+4" - { check("-32x+4+64x²", Square(Diff(VarPart(8, 1), VarPart(2, 0)))) }
      "64x²-32x-4" - { check("-32x-4+64x²", NotFactorizable(VarPart(64, 2), VarPart(-32, 1), VarPart(-4, 0))) }
      "64x²-4" - { check("-4+64x²", Product(Sum(VarPart(8, 1), VarPart(2, 0)), Diff(VarPart(8, 1), VarPart(2, 0)))) }

    }

    'testSimple{
      val Parsed.Success(parsed, _) = ArithmeticParser("32x+64x^2+4")
      assert(parsed == Seq(VarPart(64, 2), VarPart(32, 1), VarPart(4, 0)))

      val Parsed.Success(value, _) = FactorizerParser(parsed)
      assert(value.toString == "(8x + 2)^2"); (parsed.toString, value.toString).toString
    }
  }
}
