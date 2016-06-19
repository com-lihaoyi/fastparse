package pythonparse
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import sys.process._
import java.nio.file._
import utest._
import fastparse.all._
/**
 * Load external Python code and force feed it through the parser
 * to find out where it blows up
 */
object ProjectTests extends TestSuite{

  def check(ignored: Seq[String] = Nil)(implicit testPath: utest.framework.TestPath) = {
    val repo = "https://github.com/" + testPath.value.last
    val name = repo.split("/").last
    val path = Paths.get("pythonparse/jvm/target/repos/" + name)
    if (!Files.exists(path)) {
      println("Cloning")
      Seq("git", "clone", repo, path.toString, "--depth", "1").!
    }
    def listFiles(s: java.io.File): Iterator[String] = {
      val (dirs, files) = Option(s.listFiles()).toIterator
        .flatMap(_.toIterator)
        .partition(_.isDirectory)

      files.map(_.getPath) ++ dirs.flatMap(listFiles)
    }

    val pythonFiles: Seq[String] = listFiles(new java.io.File(path.toString))
            .filter(path => path.toString.endsWith(".py") && !ignored.exists(path.endsWith))
            .map(_.toString)
            .toSeq

    val grouped = Await.result(Future.sequence(pythonFiles.map { p =>
      Future {
        print("-")
        (Seq("python", "pythonparse/jvm/src/test/resources/pythonparse/parse.py", p).!, p)
      }
    }), Duration.Inf).groupBy(_._1).mapValues(_.map(_._2))
    val selfParsed = grouped(0) groupBy { x =>
      print(".")
      pythonparse.Statements.file_input.parse(new String(Files.readAllBytes(Paths.get(x)))).getClass
    }

    selfParsed.get(classOf[Parsed.Failure]) match{
      case None => (grouped.mapValues(_.length), selfParsed.mapValues(_.length))
      case Some(xs) => throw new Exception(xs.mkString("\n"))
    }

  }
  val tests = TestSuite{
    "dropbox/changes" - check()
    "django/django" - check(
      ignored = Seq("tests/i18n/test_compilation.py")
    )
    "mitsuhiko/flask" - check()
    "zulip/zulip" - check()
    "ansible/ansible"- check()
    "kennethreitz/requests" - check()

    'bench{
      val path = "pythonparse/jvm/src/test/resources/pythonparse/bench.py"
      val data = Files.readAllBytes(Paths.get(path))
      val code = new String(data)
      (pythonparse.Statements.file_input ~ End).parse(code).get
    }
  }
}
