package fastparse

import fastparse.utils.MacroUtils

object CharPredicates{

  // Not available in Scala.js
  lazy val isPrintableChar = MacroUtils.preCompute{c =>
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
    !java.lang.Character.isSurrogate(c) &&
    block != null &&
    block != java.lang.Character.UnicodeBlock.SPECIALS
  }
  lazy val bit1 = MacroUtils.preCompute{c => (c.getType | 1) != 0}
  lazy val bit2 = MacroUtils.preCompute{c => (c.getType | 2) != 0}
  lazy val bit3 = MacroUtils.preCompute{c => (c.getType | 4) != 0}
  lazy val bit4 = MacroUtils.preCompute{c => (c.getType | 8) != 0}
  lazy val bit5 = MacroUtils.preCompute{c => (c.getType | 16) != 0}

  def boolToInt(b: Boolean) = if (b) 1 else 0
  lazy val lookupInt = Array.tabulate(Char.MaxValue+1){ i0 =>
    val i = i0.toChar
    (boolToInt(bit1(i)) << 0) |
    (boolToInt(bit2(i)) << 1) |
    (boolToInt(bit3(i)) << 2) |
    (boolToInt(bit4(i)) << 3) |
    (boolToInt(bit5(i)) << 4)
  }


  def getCharType(c: Char) = lookupInt(c)
  def isMathSymbol(c: Char) = getCharType(c) == Character.MATH_SYMBOL
  def isOtherSymbol(c: Char) = getCharType(c) == Character.OTHER_SYMBOL
  def isLetter(c: Char) = {
    ((((1 << Character.UPPERCASE_LETTER) |
      (1 << Character.LOWERCASE_LETTER) |
      (1 << Character.TITLECASE_LETTER) |
      (1 << Character.MODIFIER_LETTER) |
      (1 << Character.OTHER_LETTER)) >> getCharType(c)) & 1) != 0
  }

  def isDigit(c: Char) = getCharType(c) == Character.DECIMAL_DIGIT_NUMBER
  def isLower(c: Char) = getCharType(c) == Character.LOWERCASE_LETTER
  def isUpper(c: Char) = getCharType(c) == Character.UPPERCASE_LETTER
}