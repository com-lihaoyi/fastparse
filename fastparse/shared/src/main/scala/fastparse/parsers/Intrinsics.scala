package fastparse.parsers
import acyclic.file
import fastparse.utils.Utils._
import fastparse.core.{ParseCtx, Parsed, Parser, Precedence}
import fastparse.utils.{ElemSetHelper, ReprOps, Utils}

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {
  type Generator[Elem] = (Elem => Unit) => Unit
  abstract class PrecomputableParser[Elem, Repr](generator: Generator[Elem])
                                                (implicit val helper: ElemSetHelper[Elem],
                                                 val repr: ReprOps[Elem, Repr])
    extends Parser[Unit, Elem, Repr]{

    protected[this] val uberSet: Utils.BitSet[Elem] = BitSet(generator)
  }

  abstract class ElemSet[Elem, Repr](generatorOrPred: Generator[Elem])
                                    (implicit helper: ElemSetHelper[Elem],
                                     repr: ReprOps[Elem, Repr])
      extends PrecomputableParser[Elem, Repr](generatorOrPred){


    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val input = cfg.input
      if (!input.isReachable(index)) fail(cfg.failure, index)
      else if (uberSet(input(index))) success(cfg.success, (), index + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
  }


  class PredGenerator[Elem](predicate: Elem => Boolean)
                           (implicit helper: ElemSetHelper[Elem]) extends Generator[Elem]{
    def apply(callback: Elem => Unit): Unit = {
      helper.generateValues{ v => if(predicate(v)) callback(v)}
    }
  }

  /**
   * Parses a single character if it passes the predicate
   */
  case class ElemPred[Elem, Repr](name: String,
                                  predicate: Elem => Boolean)
                                 (implicit helper: ElemSetHelper[Elem],
                                  repr: ReprOps[Elem, Repr])
    extends ElemSet[Elem, Repr](new PredGenerator(predicate)){

    override def toString = s"$name($predicate)"
  }

  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class ElemIn[Elem, Repr](name: String,
                                strings: Seq[Seq[Elem]])
                               (implicit repr: ReprOps[Elem, Repr],
                                ehelper: ElemSetHelper[Elem])
      extends ElemSet[Elem, Repr](strings.iterator.map(_.iterator).flatten.foreach){
    override def toString = s"$name(${repr.literalize(repr.flatten(strings.map(repr.fromSeq)))})"
  }

  /**
   * Keeps consuming characters until the predicate [[predicate]] becomes false.
   * Functionally equivalent to using `.rep` and [[ElemPred]], but much faster.
   */
  case class ElemsWhile[Elem, Repr](name: String,
                                    predicate: Elem => Boolean,
                                    min: Int = 1)
                                   (implicit helper: ElemSetHelper[Elem],
                                    repr: ReprOps[Elem, Repr])
      extends PrecomputableParser[Elem, Repr](new PredGenerator(predicate)){

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      var curr = index
      val input = cfg.input
      while(input.isReachable(curr) && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Set.empty, false)
    }
    override def toString = s"$name($predicate)"
  }
  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into an array-backed Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  class StringInBase[Elem, Repr](ignoreCase: Boolean, strings: Repr*)
                                (implicit repr: ReprOps[Elem, Repr],
                                 helper: ElemSetHelper[Elem],
                                 ordering: Ordering[Elem])
      extends Parser[Unit, Elem, Repr] {

    private[this] val trie = new TrieNode[Elem](strings.map(repr.toArray(_): IndexedSeq[Elem]), ignoreCase)

    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      val length = trie.query(cfg.input, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Set.empty, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(repr.literalize).mkString(", ")})"
    }
  }

  case class StringIn[Elem, Repr](strings: Repr*)
                                 (implicit repr: ReprOps[Elem, Repr],
                                  helper: ElemSetHelper[Elem],
                                  ordering: Ordering[Elem]) extends StringInBase(false, strings:_*)

  case class StringInIgnoreCase[Elem, Repr](strings: Repr*)
                                           (implicit repr: ReprOps[Elem, Repr],
                                            helper: ElemSetHelper[Elem],
                                            ordering: Ordering[Elem]) extends StringInBase(true, strings:_*)
}
