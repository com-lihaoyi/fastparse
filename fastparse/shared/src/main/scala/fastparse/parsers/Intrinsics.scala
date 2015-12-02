package fastparse.parsers
import acyclic.file
import fastparse.Utils._
import fastparse.core.{Precedence, ParseCtx, Parsed, Parser}
import fastparse.Utils

/**
 * High-performance intrinsics for parsing common patterns. All
 * of these have equivalent to constructs that can be put together
 * using a combination of "string"s, p1 | p2, and p.rep, but much
 * faster or more convenient.
 */
object Intrinsics {

  abstract class CharSet(chars: Seq[Char]) extends Parser[Unit]{
    private[this] val uberSet = CharBitSet(chars)
    def parseRec(cfg: ParseCtx, index: Int) = {
      val input = cfg.input
      if (index >= input.length) fail(cfg.failure, index)
      else if (uberSet(input(index))) success(cfg.success, (), index + 1, Nil, false)
      else fail(cfg.failure, index)
    }
  }
  /**
   * Parses a single character if it passes the predicate
   */
  case class CharPred(predicate: Char => Boolean)
    extends CharSet((Char.MinValue to Char.MaxValue).filter(predicate)){}

  /**
   * Parses a single character if its contained in the lists of allowed characters
   */
  case class CharIn(strings: Seq[Char]*) extends CharSet(strings.flatten){
    override def toString = s"CharIn(${Utils.literalize(strings.flatten.mkString)})"
  }

  /**
   * Keeps consuming characters until the predicate [[pred]] becomes false.
   * Functionally equivalent to using `.rep` and [[CharPred]], but much faster.
   */
  case class CharsWhile(pred: Char => Boolean, min: Int = 1) extends Parser[Unit]{
    private[this] val uberSet = CharBitSet((Char.MinValue to Char.MaxValue).filter(pred))

    def parseRec(cfg: ParseCtx, index: Int) = {
      var curr = index
      val input = cfg.input
      while(curr < input.length && uberSet(input(curr))) curr += 1
      if (curr - index < min) fail(cfg.failure, curr)
      else success(cfg.success, (), curr, Nil, false)
    }
  }
  /**
   * Very efficiently attempts to parse a set of strings, by
   * first converting it into an array-backed Trie and then walking it once.
   * If multiple strings match the input, longest match wins.
   */
  case class StringIn(strings: String*) extends Parser[Unit]{

    private[this] val trie = new TrieNode(strings)

    def parseRec(cfg: ParseCtx, index: Int) = {
      val length = trie.query(cfg.input, index)
      if (length != -1) success(cfg.success, (), index + length + 1, Nil, false)
      else fail(cfg.failure, index)
    }
    override def toString = {
      s"StringIn(${strings.map(literalize(_)).mkString(", ")})"
    }
  }
}
