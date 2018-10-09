package fasterparser


/**
  * Fast, pre-computed character predicates for charactes from 0 to 65535
  *
  * Useful because FastParse does it's parsing character by character, so
  * although this doesn't have the full range of the java
  * `Character.getType(c: Int)` functions, it still is good enough for
  * a wide range of use cases
  */
object CharPredicates{

  def isMathSymbol(c: Char) = Character.getType(c) == Character.MATH_SYMBOL
  def isOtherSymbol(c: Char) = Character.getType(c) == Character.OTHER_SYMBOL
  def isLetter(c: Char) = {
    ((((1 << Character.UPPERCASE_LETTER) |
      (1 << Character.LOWERCASE_LETTER) |
      (1 << Character.TITLECASE_LETTER) |
      (1 << Character.MODIFIER_LETTER) |
      (1 << Character.OTHER_LETTER)) >> Character.getType(c)) & 1) != 0
  }

  def isPrintableChar(c: Char) = {
    val block = java.lang.Character.UnicodeBlock.of(c)
    !java.lang.Character.isISOControl(c) &&
      !java.lang.Character.isSurrogate(c) &&
      block != null &&
      block != java.lang.Character.UnicodeBlock.SPECIALS
  }
  def isDigit(c: Char) = Character.isDigit(c)
  def isLower(c: Char) = Character.isLowerCase((c))
  def isUpper(c: Char) = Character.isUpperCase(c)
}