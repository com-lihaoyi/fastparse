package fastparse.internal

import fastparse.ParsingRun

object NonMarcroImpls {

  def literalStrNonMacro(s1: String)(ctx1: ParsingRun[Any]): ParsingRun[Unit] = {
    val index = ctx1.index
    val res =
      if (Util.startsWith(ctx1.input, s1, index)) ctx1.freshSuccessUnit(index + s1.length)
      else ctx1.freshFailure().asInstanceOf[ParsingRun[Unit]]
    if (ctx1.verboseFailures) ctx1.aggregateTerminal(index, () => Util.literalize(s1))
    res
  }

}
