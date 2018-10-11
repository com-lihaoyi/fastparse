package fasterparser

import fasterparser.Parsing._

import scala.annotation.{Annotation, switch, tailrec}
/**
  * No-op whitespace syntax that doesn't consume anything
  */
object NoWhitespace {
  implicit object noWhitespaceImplicit extends (Parse[_] => Parse[Unit]){
    def apply(cfg: Parse[_]) = fasterparser.Parsing.Pass(cfg)
  }
}

/**
  * Whitespace syntax that consumes only single-line " " and "\t" whitespace
  * characters.
  */
object SingleLineWhitespace {
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = CharsWhileIn(" \t", 0)(cfg)
}
/**
  * Whitespace syntax that consumes both single-line " " and "\t" and multiline
  * "\r" and "\n" whitespace characters.
  */
object MultiLineWhitespace {
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = CharsWhileIn(" \t\r\n", 0)(cfg)
}


/**
  * Whitespace syntax that supports // line-comments and /* */
  * multiline-comments, *without nesting* of /* */ comments, as is the case
  * in the Java programming language
  */
object JavaWhitespace{
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = {
    val input = cfg.input
    @tailrec def rec(current: Int, state: Int): Parse[Unit] = {
      if (!input.isReachable(current)) cfg.prepareSuccess((), current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '/' => rec(current + 1, state = 2)
              case _ => cfg.prepareSuccess((), current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
          case 2 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 1)
              case _ => cfg.prepareSuccess((), current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 => rec(current + 1, state = if (currentChar == '/') 0 else 3)
        }
      }
    }
    rec(current = cfg.index, state = 0)
  }
}
/**
  * Whitespace syntax that supports # line-comments, as in the case in
  * programming languages such as Bash, Ruby, or Python
  */
object ScriptWhitespace{
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = {
    val input = cfg.input
    @tailrec def rec(current: Int, state: Int): Parse[Unit] = {
      if (!input.isReachable(current)) cfg.prepareSuccess((), current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '#' => rec(current + 1, state = 1)
              case _ => cfg.prepareSuccess((), current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
        }
      }
    }
    rec(current = cfg.index, state = 0)
  }
}

/**
  * Whitespace syntax that supports // and # line comments, and /* */
  * multiline-comments, but *without* nesting of /* */ comments. This is the
  * case in the Jsonnet programming language
  */
object JsonnetWhitespace{
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = {
    val input = cfg.input
    @tailrec def rec(current: Int, state: Int): Parse[Unit] = {
      if (!input.isReachable(current)) cfg.prepareSuccess((), current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '#' => rec(current + 1, state = 1)
              case '/' => rec(current + 1, state = 2)
              case _ => cfg.prepareSuccess((), current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
          case 2 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 1)
              case '*' => rec(current + 1, state = 3)
              case _ => cfg.prepareSuccess((), current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 => rec(current + 1, state = if (currentChar == '/') 0 else 3)
        }
      }
    }
    rec(current = cfg.index, state = 0)
  }
}

/**
  * Whitespace syntax that supports // line-comments and /* */
  * multiline-comments, *including nesting* of /* */ comments, as is the case
  * in the Scala programming language
  */
object ScalaWhitespace {
  implicit def whitespace(cfg: Parse[_]): Parse[Unit] = {
    val input = cfg.input

    @tailrec def rec(current: Int, state: Int, nesting: Int): Parse[Unit] = {
      if (!input.isReachable(current)) cfg.prepareSuccess((), current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state, 0)
              case '/' => rec(current + 1, state = 2, 0)
              case _ => cfg.prepareSuccess((), current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state, 0)
          case 2 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 1, 0)
              case '*' => rec(current + 1, state = 3, nesting + 1)
              case _ => cfg.prepareSuccess((), current - 1)
            }
          case 3 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 2, nesting)
              case '*' => rec(current + 1, state = 4 , nesting)
              case _ => rec(current + 1, state = state, nesting)
            }
          case 4 =>
            if (currentChar == '/') rec(current + 1, state = if (nesting == 1) 0 else 3 , nesting - 1)
            else rec(current + 1, state = 3, nesting)
        }
      }
    }
    rec(current = cfg.index, state = 0, nesting = 0)
  }

}

