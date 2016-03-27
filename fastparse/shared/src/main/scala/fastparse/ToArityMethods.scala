package fastparse

import parsers.Transformers.Mapper
import core.Parser

/** Definitions of the `to` method variants for arities up to 22. This trait is meant as a mix-in for [[Api]]
  * so as to keep the boilerplate out of `Api.scala` itself. */
trait ToArityMethods {
  implicit final class To0(val p: Parser[Unit]) {
    def to[B](f: => B): Parser[B] = Mapper(p, (_: Unit) => f)
  }

  // This is essentially just an alias for `map`, but it makes a consistent story
  implicit final class To1[A1](val p: Parser[A1]) {
    def to[B](f: A1 => B): Parser[B] = Mapper(p, f)
  }

  implicit final class To2[A1, A2](val p: Parser[(A1, A2)]) {
    def to[B](f: (A1, A2) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To3[A1, A2, A3](val p: Parser[(A1, A2, A3)]) {
    def to[B](f: (A1, A2, A3) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To4[A1, A2, A3, A4](val p: Parser[(A1, A2, A3, A4)]) {
    def to[B](f: (A1, A2, A3, A4) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To5[A1, A2, A3, A4, A5](val p: Parser[(A1, A2, A3, A4, A5)]) {
    def to[B](f: (A1, A2, A3, A4, A5) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To6[A1, A2, A3, A4, A5, A6](val p: Parser[(A1, A2, A3, A4, A5, A6)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To7[A1, A2, A3, A4, A5, A6, A7](val p: Parser[(A1, A2, A3, A4, A5, A6, A7)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To8[A1, A2, A3, A4, A5, A6, A7, A8](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To9[A1, A2, A3, A4, A5, A6, A7, A8, A9](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To10[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To11[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To12[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To13[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To14[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To15[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To16[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To17[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To18[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To19[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To20[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To21[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21) => B): Parser[B] = Mapper(p, f.tupled)
  }

  implicit final class To22[A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22](val p: Parser[(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22)]) {
    def to[B](f: (A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11, A12, A13, A14, A15, A16, A17, A18, A19, A20, A21, A22) => B): Parser[B] = Mapper(p, f.tupled)
  }
}
