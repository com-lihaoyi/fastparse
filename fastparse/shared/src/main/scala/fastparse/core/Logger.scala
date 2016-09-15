package fastparse.core

case class Logger(f: String => Unit)
object Logger {
  implicit val stdout = Logger(println)
}
