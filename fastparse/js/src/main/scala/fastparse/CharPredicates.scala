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
  lazy val isMathSymbol = MacroUtils.preCompute(_.getType == Character.MATH_SYMBOL)
  lazy val isOtherSymbol = MacroUtils.preCompute(_.getType == Character.OTHER_SYMBOL)
}