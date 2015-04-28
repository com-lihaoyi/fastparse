package parsing
import utest._
object ParsingTests extends TestSuite{
  trait Sequencer[T, V, R]{
    def apply(t: T, v: V): R
  }
  object Sequencer extends LowPriSequencer{
    def apply[T, V, R](f: (T, V) => R) = new Sequencer[T, V, R]{
      def apply(t: T, v: V): R = f(t, v)
    }
    implicit def SingleSequencer[T]: Sequencer[Unit, T, T] = Sequencer{case ((), t) => t }
  }
  trait LowPriSequencer extends LowerPriSequencer{
    implicit def UnitSequencer[T]: Sequencer[T, Unit, T] = Sequencer{case ((t), ()) => t }
  }
  trait LowerPriSequencer extends LowestPriSequencer{
    implicit def Sequencer2[T1, T2, D]: Sequencer[(T1, T2), D, (T1, T2, D)] = Sequencer{case ((t1, t2), d) => (t1, t2, d) }
    implicit def Sequencer3[T1, T2, T3, D]: Sequencer[(T1, T2, T3), D, (T1, T2, T3, D)] = Sequencer{case ((t1, t2, t3), d) => (t1, t2, t3, d) }
    implicit def Sequencer4[T1, T2, T3, T4, D]: Sequencer[(T1, T2, T3, T4), D, (T1, T2, T3, T4, D)] = Sequencer{case ((t1, t2, t3, t4), d) => (t1, t2, t3, t4, d) }
    implicit def Sequencer5[T1, T2, T3, T4, T5, D]: Sequencer[(T1, T2, T3, T4, T5), D, (T1, T2, T3, T4, T5, D)] = Sequencer{case ((t1, t2, t3, t4, t5), d) => (t1, t2, t3, t4, t5, d) }
    implicit def Sequencer6[T1, T2, T3, T4, T5, T6, D]: Sequencer[(T1, T2, T3, T4, T5, T6), D, (T1, T2, T3, T4, T5, T6, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6), d) => (t1, t2, t3, t4, t5, t6, d) }
    implicit def Sequencer7[T1, T2, T3, T4, T5, T6, T7, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7), D, (T1, T2, T3, T4, T5, T6, T7, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6, t7), d) => (t1, t2, t3, t4, t5, t6, t7, d) }
    implicit def Sequencer8[T1, T2, T3, T4, T5, T6, T7, T8, D]: Sequencer[(T1, T2, T3, T4, T5, T6, T7, T8), D, (T1, T2, T3, T4, T5, T6, T7, T8, D)] = Sequencer{case ((t1, t2, t3, t4, t5, t6, t7, t8), d) => (t1, t2, t3, t4, t5, t6, t7, t8, d) }
  }
  trait LowestPriSequencer{
    implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = null
  }
  class P[T]{
    def ~[V, R](other: P[V])(implicit sum: Sequencer[T, V, R]): P[R] = new P[R]
    def rep[R](implicit rep: Repeater[T, R]): P[R] = new P[R]
  }
  trait Repeater[T, R]{
    def apply(i: Iterator[T]): R
  }
  object Repeater extends LowPriRepeater{
    def apply[T, R](f: Iterator[T] => R) = new Repeater[T, R]{
      def apply(i: Iterator[T]): R = f(i)
    }
    implicit def UnitRepeater = Repeater[Unit, Unit](_ => ())
  }
  trait LowPriRepeater{
    implicit def GenericRepeater[T] = Repeater[T, Seq[T]](_.toVector)
  }
  def P[T] = new P[T]
  val p1: P[Unit] = P[Unit] ~ P[Unit]
  val p2: P[Int] = P[Unit] ~ P[Int]
  val p3: P[Int] = P[Int] ~ P[Unit]
  val p4: P[(Char, Int)] = P[Char] ~ P[Int]
  val p5: P[(Char, Int, String)] = P[Char] ~ P[Int] ~ P[String]
  val p6: P[(Char, Int, String)] = P[Char] ~ P[Int] ~ P[Unit] ~ P[String] ~ P[Unit]
  val p7: P[(Char, Int, String, (Int, Int))] = P[Char] ~ P[Int] ~ P[Unit] ~ P[String] ~ P[Unit] ~ P[(Int, Int)]

  class R[T]
  val p8: P[Unit] = P[Unit].rep
  val p9: P[Seq[Int]] = P[Int].rep
  import Res.Success
  def check[T](parser: Parser[T], input: (String, Int), rhs: Res[T]) = {
    val (str, index) = input
    val parsed = parser.parse(str, index)
    assert({parser; str; parsed} == rhs)
  }
  val tests = TestSuite{


    'literal{
//      check("Hello WOrld!", ("Hello", 0), Failure(0))
      check("Hello", ("Hello WOrld!", 0), Success("Hello", 5))
//      check("Hello", ("Hello WOrld!", 5), Failure(5))
      check(" WO", ("Hello WOrld!", 5), Success(" WO", 8))
    }
    'repeat{
      check("Hello".rep, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
      check("Hello".rep, ("HelloHello!", 2), Success(Seq(), 2))
      check("Hello".rep, ("HelloHello!", 5), Success(Seq("Hello"), 10))

      check("Hello".rep1, ("HelloHello!", 0), Success(Seq("Hello", "Hello"), 10))
//      check("Hello".rep1, ("HelloHello!", 2), Failure(2))
    }
    'either{
      check("Hello" | "Bye", ("HelloBye", 0), Success("Hello", 5))
      check("Hello" | "Bye", ("HelloBye", 5), Success("Bye", 8))
//      check("Hello" | "Bye", ("HelloBye", 2), Failure(2))
      check(("Hello" | "Bye").rep, ("HelloBye", 0), Success(Seq("Hello", "Bye"), 8))
    }
    'sequence{
      check("Hello" ~ "Bye", ("HelloBye", 0), Success(("Hello", "Bye"), 8))
      check("Hello" ~ "Bye" ~ "!", ("HelloBye!", 0), Success((("Hello", "Bye"), "!"), 9))
//      check("Hello" ~ "Bye", ("Hello", 0), Failure(5))
//      check("Hello" ~ "Bye", ("Bye", 0), Failure(0))
    }
  }
}
