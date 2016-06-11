package fastparse.parsers
import acyclic.file
import fastparse.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.{ParserHelper, Utils}

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {

  abstract class ElemSet[ElemType, R](elems: Seq[ElemType])
                                     (implicit helper: ElemHelper[ElemType],
                                               ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, R]{
    private[this] val uberSet = BitSet(elems)
    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      val input = cfg.input
      if (index >= input.length) fail(cfg.failure, index)
      else if (uberSet(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
  }
  /**
   * Parses a single character if it passes the predicate
   */
  case class ElemPred[ElemType, R](predicate: ElemType => Boolean)
                                  (implicit helper: ElemHelper[ElemType],
                                            ordering: Ordering[ElemType])
    extends ElemSet[ElemType, R](helper.allValues.filter(predicate)){}

  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class ElemIn[ElemType, R](strings: IndexedSeq[ElemType]*)
                                (implicit helper: ParserHelper[ElemType],
                                          ehelper: ElemHelper[ElemType],
                                          ordering: Ordering[ElemType])
      extends ElemSet[ElemType, R](strings.flatten){
    override def toString = s"CharIn(${helper.literalize(strings.flatten.toIndexedSeq)})"
  }

  /**
   * Keeps consuming characters until the predicate [[pred]] becomes false.
   * Functionally equivalent to using `.rep` and [[ElemPred]], but much faster.
   */
  case class ElemsWhile[ElemType, R](pred: ElemType => Boolean, min: Int = 1)
                                    (implicit helper: ElemHelper[ElemType],
                                              ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, R]{
    private[this] val uberSet = BitSet(helper.allValues.filter(pred))

    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      var curr = index
      val input = cfg.input
      while(curr < input.length && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, false)
    }
  }
  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into an array-backed Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  case class StringIn[ElemType, R](strings: IndexedSeq[ElemType]*)
                                  (implicit helper: ParserHelper[ElemType],
                                            ehelper: ElemHelper[ElemType],
                                            ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, R] {

    private[this] val trie = new TrieNode[ElemType](strings)

    def parseRec(cfg: ParseCtx[ElemType], index: Int) = {
      val length = trie.query(cfg.input, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(helper.literalize).mkString(", ")})"
    }
  }
}
