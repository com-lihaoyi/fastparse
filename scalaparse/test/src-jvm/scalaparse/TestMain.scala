package scalaparse
import fasterparser._, Parsing._

/**
  * Created by lihaoyi on 13/5/17.
  */
object TestMain {
  def main(args: Array[String]): Unit = {
    val input = """
    | object X{
    | {
    |   val x = 1
    |   ;
    | """.stripMargin
    val res = Parse(input).read(scalaparse.Scala.CompilationUnit(_))
//    val fail = res.asInstanceOf[Result.Failure]
    println(res)
//    for(frame <- fail.extra.traced.stack){
//      println(Result.Failure.formatParser(frame.parser, fail.extra.input, frame.index))
//    }
//    println(fail.msg)
  }
}
