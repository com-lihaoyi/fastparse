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
    'fastparse - bench{
      fastparse.parse(txt, test.fastparse.Json.jsonExpr(_))
    }
    'circe - bench{
      io.circe.parser.parse(txt).asInstanceOf[Right[_, _]]
    }
    'argonaut - bench{
      argonaut.Parse.parse(txt).asInstanceOf[Right[_, _]]
    }
    'ujson - bench{
      ujson.read(txt)
    }
    'json4s - bench{
      org.json4s.native.JsonMethods.parse(txt)
    }
    'play - bench{
      play.api.libs.json.Json.parse(txt)
    }
    'combinators - bench{
      scala.util.parsing.json.JSON.parseRaw(txt).get
    }
  }
}