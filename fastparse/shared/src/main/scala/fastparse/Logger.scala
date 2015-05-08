package fastparse

import scala.annotation.implicitNotFound

@implicitNotFound("You need to specify an implicit logger, e.g. `import fastparse.Logger.stdout`")
case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}
