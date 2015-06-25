package fastparse
import acyclic.file
import scala.annotation.implicitNotFound

case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}
