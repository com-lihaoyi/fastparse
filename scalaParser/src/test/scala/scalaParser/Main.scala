package scalaParser

/**
 * Created by haoyi on 4/26/15.
 */
object Main {
  def main(args: Array[String]): Unit = {
    println("Walking...")
    val walked = parsing.RuleWalker.recurse(
      Scala.Block, Nil
    )
    println("Walked!")
    println(walked)
  }
}

