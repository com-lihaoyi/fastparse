package fastparse.parsers
import acyclic.file
import fastparse.core.Mutable
import fastparse.core.Parser
import fastparse.core.ParseCtx
import fastparse.utils.ReprOps

/**
 * Parsers that work with the output of a successful parse
 */
object Transformers {
  /**
   * Applies a transformation [[f]] to the result of [[p]]
   */
  case class Mapper[T, V, Elem, Repr](p: Parser[T, Elem, Repr], f: T => V)
                                     (implicit repr: ReprOps[Elem, Repr])
    extends Parser[V, Elem, Repr]{
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p.parseRec(cfg, index) match{
        case s: Mutable.Success[T, Elem, Repr] => success(s, f(s.value), s.index, s.traceParsers, s.cut)
        case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth)
      }
    }
    override def toString = p.toString
  }

  case class FlatMapped[T, V, Elem, Repr](p1: Parser[T, Elem, Repr], func: T => Parser[V, Elem, Repr])
                                         (implicit repr: ReprOps[Elem, Repr])
    extends Parser[V, Elem, Repr] {
    def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p1.parseRec(cfg, index) match{
        case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T, Elem, Repr] =>
          val sCut = s.cut
          val res = func(s.value).parseRec(cfg, s.index)
          res.cut = sCut
          res
      }
    }
    override def toString = p1.toString
  }

  case class Filtered[T, Elem, Repr](p: Parser[T, Elem, Repr], predicate: T => Boolean)
                                    (implicit repr: ReprOps[Elem, Repr])
    extends Parser[T, Elem, Repr] {
    override def parseRec(cfg: ParseCtx[Elem, Repr], index: Int) = {
      p.parseRec(cfg, index) match{
        case f: Mutable.Failure[Elem, Repr] => failMore(f, index, cfg.logDepth, cut = false)
        case s: Mutable.Success[T, Elem, Repr] =>
          if (predicate(s.value)) s
          else fail(cfg.failure, index, s.traceParsers, cut = s.cut)
      }
    }

    override def toString: String = s"$p.filter($predicate)"
  }
}
