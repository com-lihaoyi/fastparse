package scalaParser
import acyclic.file
import macros.Macros._
import org.parboiled2._

import scala.language.implicitConversions


abstract class Core extends Parser with syntax.Basic with syntax.Literals with syntax.Identifiers {
  // Aliases for common things. These things are used in almost every parser
  // in the file, so it makes sense to keep them short.
  type R0 = Rule0
  /**
   * Parses all whitespace, excluding newlines. This is only
   * really useful in e.g. {} blocks, where we want to avoid
   * capturing newlines so semicolon-inference would work
   */
  def WS = rule( rep(Basic.WhitespaceChar | Literals.Comment) )

  /**
   * Parses whitespace, including newlines.
   * This is the default for most things
   */
  def WL = rule( rep(Basic.WhitespaceChar | Literals.Comment | Basic.Newline) )


  /**
   * By default, all strings and characters greedily
   * capture all whitespace immediately before the token.
   */
  implicit def wspStr(s: String): R0 = {
    rule( WL ~ str(s) )
  }
  implicit def wspCh(s: Char): R0 = {
    rule( WL ~ ch(s) )
  }

  /**
   * Most keywords don't just require the correct characters to match,
   * they have to ensure that subsequent characters *don't* match in
   * order for it to be a keyword. This enforces that rule for key-words
   * (W) and key-operators (O) which have different non-match criteria.
   */
  object KeyWordOperators {
    private[this] def W(s: String) = rule( WL ~ Key.W(s) )
    private[this] def O(s: String) = rule( WL ~ Key.O(s) )

    def `=>` = rule( O("=>") | O("⇒") )
    def `<-` = rule( O("<-") | O("←") )
    def `:` = O(":")
    def `=` = O("=")
    def `@` = O("@")
    def `_` = W("_")
    def `this` = W("this")
    def `type` = W("type")
    def `val` = W("val")
    def `var` = W("var")
    def `def` = W("def")
    def `with` = W("with")
    def `package` = W("package")
    def `object` = W("object")
    def `class` = W("class")
    def `case` = W("case")
    def `trait` = W("trait")
    def `extends` = W("extends")
    def `implicit` = W("implicit")
    def `try` = W("try")
    def `new` = W("new")
    def `macro` = W("macro")
    def `import` = W("import")
    def `else` = W("else")
    def `super` = W("super")
    def `catch` = W("catch")
    def `finally` = W("finally")
    def `do` = W("do")
    def `yield` = W("yield")
    def `while` = W("while")
    def `<%` = O("<%")
    def `override` = W("override")
    def `#` = O("#")
    def `forSome` = W("forSome")
    def `for` = W("for")
    def `abstract` = W("abstract")
    def `throw` = W("throw")
    def `return` = W("return")
    def `lazy` = W("lazy")
    def `if` = W("if")
    def `match` = W("match")
    def `>:` = O(">:")
    def `<:` = O("<:")
    def `final` =  W("final")
    def `sealed` = W("sealed")
    def `private` = W("private")
    def `protected` = W("protected")
  }

  import KeyWordOperators._
  import KeyWordOperators.`_`

  def `_*` = rule( `_` ~ "*" )
  def `}` = rule( opt(Semis) ~ '}' )
  def `{` = rule( '{' ~ opt(Semis) )
  /**
   * helper printing function
   */
  def pr(s: String) = rule( run(println(s"LOGGING $cursor: $s")) )

  def Id = rule( WL ~ Identifiers.Id )
  def VarId = rule( WL ~ Identifiers.VarId )
  def Literal = rule( WL ~ Literals.Literal )
  def Semi = rule( WS ~ Basic.Semi )
  def Semis = rule( rep1(Semi) )
  def Newline = rule( WL ~ Basic.Newline )

  def QualId = rule( WL ~ rep1Sep(Id, '.') )
  def Ids = rule( rep1Sep(Id, ',') )

  def NotNewline: R0 = rule( &( WS ~ !Basic.Newline ) )
  def OneNLMax: R0 = {
    def WSChar = rule( rep(Basic.WhitespaceChar) )
    def ConsumeComments = rule( rep(WSChar ~ Literals.Comment ~ WSChar ~ Basic.Newline) )
    rule( WS ~ opt(Basic.Newline) ~ ConsumeComments ~ NotNewline )
  }
  def StableId: R0 = {
    def ClassQualifier = rule( '[' ~ Id ~ ']' )
    def ThisSuper = rule( `this` | `super` ~ opt(ClassQualifier) )
    rule( rep(Id ~ '.') ~ ThisSuper ~ rep('.' ~ Id) | Id ~ rep('.' ~ Id) )
  }
}
