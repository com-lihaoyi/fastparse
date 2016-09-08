package fastparse.parsers
import acyclic.file
import fastparse.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.{ElemSetHelper, ElemTypeFormatter, Utils}

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {

  abstract class ElemSet[ElemType, Repr](elems: Seq[ElemType])
                                        (implicit helper: ElemSetHelper[ElemType],
                                                  ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, Repr]{
    private[this] val uberSet = BitSet(elems)
    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (uberSet(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
  }
  /**
   * Parses a single character if it passes the predicate
   */
  case class ElemPred[ElemType, Repr](name: String,
                                      predicate: ElemType => Boolean)
                                     (implicit helper: ElemSetHelper[ElemType],
                                               ordering: Ordering[ElemType])
    extends ElemSet[ElemType, Repr](helper.allValues.filter(predicate)){

    override def toString = s"$name($predicate)"
  }

  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class ElemIn[ElemType, Repr](name: String,
                                    strings: IndexedSeq[ElemType]*)
                                   (implicit formatter: ElemTypeFormatter[ElemType],
                                    ehelper: ElemSetHelper[ElemType],
                                    ordering: Ordering[ElemType])
      extends ElemSet[ElemType, Repr](strings.flatten){
    override def toString = s"$name(${formatter.literalize(strings.flatten.toIndexedSeq)})"
  }

  /**
   * Keeps consuming characters until the predicate [[pred]] becomes false.
   * Functionally equivalent to using `.rep` and [[ElemPred]], but much faster.
   */
  case class ElemsWhile[ElemType, Repr](name: String,
                                        pred: ElemType => Boolean, min: Int = 1)
                                       (implicit helper: ElemSetHelper[ElemType],
                                                 ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, Repr]{
    private[this] val uberSet = BitSet(helper.allValues.filter(pred))

    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      var curr = index
      val input = cfg.input
      while(input.isReachable(curr) && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, false)
    }
    override def toString = s"$name($pred)"
  }
  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into an array-backed Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  case class StringIn[ElemType, Repr](strings: IndexedSeq[ElemType]*)
                                     (implicit formatter: ElemTypeFormatter[ElemType],
                                      helper: ElemSetHelper[ElemType],
                                      ordering: Ordering[ElemType])
      extends Parser[Unit, ElemType, Repr] {

    private[this] val trie = new TrieNode[ElemType](strings)

    def parseRec(cfg: ParseCtx[ElemType, Repr], index: Int) = {
      val length = trie.query(cfg.input, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(formatter.literalize).mkString(", ")})"
    }
  }
}
