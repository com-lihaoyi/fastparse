package scalaparse
import fastparse.core.Parsed

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
    val res = scalaparse.Scala.CompilationUnit.parse(input)
    val fail = res.asInstanceOf[fastparse.all.Parsed.Failure]
    for(frame <- fail.extra.traced.stack){
      println(Parsed.Failure.formatParser(frame.parser, fail.extra.input, frame.index))
    }
    println(fail.msg)
  }
}
