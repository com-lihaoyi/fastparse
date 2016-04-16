package cssparse

object TestUtilJVM {
  def checkPrinting(input: String, tag: String = "") = {
    import java.io.StringReader
    import org.w3c.css.sac.InputSource
    import org.w3c.css.sac._
    import com.steadystate.css.parser._

    val parsedInput = PrettyPrinter.printRuleList(CssParser.ruleList.parse(input).get.value)
    val source = new InputSource(new StringReader(parsedInput))
    val parser = new CSSOMParser(new SACParserCSS3())
    parser.setErrorHandler(new ErrorHandler{
      def error(ex: CSSParseException) = println("ERROR " + ex)
      def fatalError(ex: CSSParseException) = println("FATAL ERROR " + ex)
      def warning(ex: CSSParseException) = println("WARNING " + ex)
    })
    val sheet = parser.parseStyleSheet(source, null, null)
    assert(sheet != null)
  }
}
