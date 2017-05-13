package fastparse

import fastparse.utils.MacroUtils

/**
  * Fast, pre-computed character predicates for charactes from 0 to 65535
  *
  * Useful because FastParse does it's parsing character by character, so
  * although this doesn't have the full range of the java
  * `Character.getType(c: Int)` functions, it still is good enough for
  * a wide range of use cases
  */
object CharPredicates{
  lazy val isPrintableChar = MacroUtils.preCompute{c =>
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
    !java.lang.Character.isSurrogate(c) &&
    block != null &&
    block != java.lang.Character.UnicodeBlock.SPECIALS
  }
  // a character's type only seems to go from 0 to 30; hence we
  // can store each in 5 bits. Here we make use of the existing
  // 1-bit-per-char preComputed bitmaps we already had, and simply
  // join them back in charTypeLookup in order to get an array of
  // character types from 0 to 65535
  //
  // All this is done lazily at runtime, so if you don't use it
  // you don't pay any cost. On Scala.js, this all gets
  // dead-code-eliminated if it isn't used, so you don't pay any code-size cost

  lazy val bit1 = MacroUtils.preCompute{c => (c.getType & 1) != 0}
  lazy val bit2 = MacroUtils.preCompute{c => (c.getType & 2) != 0}
  lazy val bit3 = MacroUtils.preCompute{c => (c.getType & 4) != 0}
  lazy val bit4 = MacroUtils.preCompute{c => (c.getType & 8) != 0}
  lazy val bit5 = MacroUtils.preCompute{c => (c.getType & 16) != 0}

  def boolToInt(b: Boolean, n: Int) = if (b) 1 << n else 0

  lazy val charTypeLookup: IndexedSeq[Int] = Array.tabulate(Char.MaxValue+1){ i0 =>
    val i = i0.toChar
    boolToInt(bit1(i), 0) |
    boolToInt(bit2(i), 1) |
    boolToInt(bit3(i), 2) |
    boolToInt(bit4(i), 3) |
    boolToInt(bit5(i), 4)
  }



  def isMathSymbol(c: Char) = charTypeLookup(c) == Character.MATH_SYMBOL
  def isOtherSymbol(c: Char) = charTypeLookup(c) == Character.OTHER_SYMBOL
  def isLetter(c: Char) = {
    ((((1 << Character.UPPERCASE_LETTER) |
      (1 << Character.LOWERCASE_LETTER) |
      (1 << Character.TITLECASE_LETTER) |
      (1 << Character.MODIFIER_LETTER) |
      (1 << Character.OTHER_LETTER)) >> charTypeLookup(c)) & 1) != 0
  }

  def isDigit(c: Char) = charTypeLookup(c) == Character.DECIMAL_DIGIT_NUMBER
  def isLower(c: Char) = charTypeLookup(c) == Character.LOWERCASE_LETTER
  def isUpper(c: Char) = charTypeLookup(c) == Character.UPPERCASE_LETTER
}