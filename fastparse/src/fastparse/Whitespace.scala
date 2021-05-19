package fastparse

import fastparse._
import fastparse.internal.Util

import scala.annotation.{Annotation, switch, tailrec}
/**
  * No-op whitespace syntax that doesn't consume anything
  */
object NoWhitespace {
  implicit object noWhitespaceImplicit extends (ParsingRun[_] => ParsingRun[Unit]){
    def apply(ctx: ParsingRun[_]) = ctx.freshSuccessUnit()
  }
}

/**
  * Whitespace syntax that consumes only single-line " " and "\t" whitespace
  * characters.
  */
object SingleLineWhitespace {
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    var index = ctx.index
    val input = ctx.input
    
    while(
      input.isReachable(index) &&
      (input(index) match{ case ' ' | '\t' => true case _ => false})
    ) index += 1
    ctx.freshSuccessUnit(index = index)
  }
}
/**
  * Whitespace syntax that consumes both single-line " " and "\t" and multiline
  * "\r" and "\n" whitespace characters.
  */
object MultiLineWhitespace {
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    var index = ctx.index
    val input = ctx.input

    while(
      input.isReachable(index) &&
        (input(index) match{ case ' ' | '\t' | '\r' | '\n' => true case _ => false})
    ) index += 1
    ctx.freshSuccessUnit(index = index)
  }
}

/**
  * Whitespace syntax that supports # line-comments, as in the case in
  * programming languages such as Bash, Ruby, or Python
  */
object ScriptWhitespace{
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) ctx.freshSuccessUnit(current)
      else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '#' => rec(current + 1, state = 1)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
        }
      }
    }
    rec(current = ctx.index, state = 0)
  }
}

/**
  * Whitespace syntax that supports // line-comments and /* */
  * multiline-comments, *without nesting* of /* */ comments, as is the case
  * in the Java programming language
  */
object JavaWhitespace{
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index
    @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state == 0 || state == 1) ctx.freshSuccessUnit(current)
        else if(state == 2)  ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/"))
          res
        }
      } else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '/' => rec(current + 1, state = 2)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
          case 2 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 1)
              case '*' => rec(current + 1, state = 3)
              case _ => ctx.freshSuccessUnit(current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 0)
              case '*' => rec(current + 1, state = 4)
              case _ => rec(current + 1, state = 3)
            }
//            rec(current + 1, state = if (currentChar == '/') 0 else 3)
        }
      }
    }
    rec(current = ctx.index, state = 0)
  }
}

/**
  * Whitespace syntax that supports // and # line comments, and /* */
  * multiline-comments, but *without* nesting of /* */ comments. This is the
  * case in the Jsonnet programming language
  */
object JsonnetWhitespace{
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index
    @tailrec def rec(current: Int, state: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state == 0 || state == 1) ctx.freshSuccessUnit(current)
        else if(state == 2)  ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/"))
          res
        }
      } else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state)
              case '#' => rec(current + 1, state = 1)
              case '/' => rec(current + 1, state = 2)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state)
          case 2 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 1)
              case '*' => rec(current + 1, state = 3)
              case _ => ctx.freshSuccessUnit(current - 1)
            }
          case 3 => rec(current + 1, state = if (currentChar == '*') 4 else state)
          case 4 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 0)
              case '*' => rec(current + 1, state = 4)
              case _ => rec(current + 1, state = 3)
            }
        }
      }
    }
    rec(current = ctx.index, state = 0)
  }
}

/**
  * Whitespace syntax that supports // line-comments and /* */
  * multiline-comments, *including nesting* of /* */ comments, as is the case
  * in the Scala programming language
  */
object ScalaWhitespace {
  implicit val whitespace: ParsingRun[_] => ParsingRun[Unit] = {implicit ctx: ParsingRun[_] =>
    val input = ctx.input
    val startIndex = ctx.index
    @tailrec def rec(current: Int, state: Int, nesting: Int): ParsingRun[Unit] = {
      if (!input.isReachable(current)) {
        if (state == 0 || state == 1) ctx.freshSuccessUnit(current)
        else if(state == 2 && nesting == 0) ctx.freshSuccessUnit(current - 1)
        else {
          ctx.cut = true
          val res = ctx.freshFailure(current)
          if (ctx.verboseFailures) ctx.setMsg(startIndex, () => Util.literalize("*/"))
          res
        }
      } else {
        val currentChar = input(current)
        (state: @switch) match{
          case 0 =>
            (currentChar: @switch) match{
              case ' ' | '\t' | '\n' | '\r' => rec(current + 1, state, 0)
              case '/' => rec(current + 1, state = 2, 0)
              case _ => ctx.freshSuccessUnit(current)
            }
          case 1 => rec(current + 1, state = if (currentChar == '\n') 0 else state, 0)
          case 2 =>
            (currentChar: @switch) match{
              case '/' =>
                if (nesting == 0) rec(current + 1, state = 1, 0)
                else rec(current + 1, state = 2, nesting)
              case '*' => rec(current + 1, state = 3, nesting + 1)
              case _ =>
                if (nesting == 0) ctx.freshSuccessUnit(current - 1)
                else rec(current + 1, state = 3, nesting)
            }
          case 3 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = 2, nesting)
              case '*' => rec(current + 1, state = 4 , nesting)
              case _ => rec(current + 1, state = state, nesting)
            }
          case 4 =>
            (currentChar: @switch) match{
              case '/' => rec(current + 1, state = if (nesting == 1) 0 else 3 , nesting - 1)
              case '*' => rec(current + 1, state = 4, nesting)
              case _ => rec(current + 1, state = 3, nesting)
            }
        }
      }
    }
    rec(current = ctx.index, state = 0, nesting = 0)
  }

}

