package compare
import utest._

object JsonBench extends TestSuite{
  def bench(f: => Unit) = {
    var start = System.currentTimeMillis()
    while(System.currentTimeMillis() - start < 10000) {
      f
    }
    start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < 10000) {
      f
      count += 1
    }
    count
  }
  val txt = scala.io.Source
    .fromInputStream(getClass.getResourceAsStream("/fastparse/test.json"))
    .mkString

  val tests = Tests{
    test("fastparse") - bench{
      fastparse.parse(txt, _root_.test.fastparse.Json.jsonExpr(_))
    }
    test("circe") - bench{
      io.circe.parser.parse(txt).asInstanceOf[Right[_, _]]
    }
    test("argonaut") - bench{
      argonaut.Parse.parse(txt).asInstanceOf[Right[_, _]]
    }
    test("ujson") - bench{
      ujson.read(txt)
    }
    test("json4s") - bench{
      org.json4s.native.JsonMethods.parse(txt)
    }
    test("play") - bench{
      play.api.libs.json.Json.parse(txt)
    }
    test("combinators") - bench{
      scala.util.parsing.json.JSON.parseRaw(txt).get
    }
  }
}