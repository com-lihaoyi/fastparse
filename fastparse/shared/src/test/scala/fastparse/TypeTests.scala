package fastparse

import fastparse.core.Implicits

/**
 * Make sure the type-level logic does the right thing. Doesn't
 * actually need to execute; compiling is enough!
 */
object TypeTests {
  class P[T]{
    def ~[V, R](other: P[V])(implicit sum: Implicits.Sequencer[T, V, R]): P[R] = new P[R]
    def rep[R](implicit rep: Implicits.Repeater[T, R]): P[R] = new P[R]
    def ?[R](implicit rep: Implicits.Optioner[T, R]): P[R] = new P[R]
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
  val p10: P[Unit] = P[Unit].?
  val p11: P[Option[Int]] = P[Int].?
}
