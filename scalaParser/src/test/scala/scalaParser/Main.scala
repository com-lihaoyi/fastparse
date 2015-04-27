package scalaParser

/**
 * Created by haoyi on 4/26/15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println(syntax.Basic.Digit.parse("a", 0))
  }
}

