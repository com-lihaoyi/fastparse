package pythonparse
import concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import java.nio.file._
import utest._
import fastparse._
/**
 * Load external Python code and force feed it through the parser
 * to find out where it blows up
 */
object ProjectTests extends TestSuite{

  def check(commitHash: String,
            ignored: Seq[String] = Nil)
           (implicit testPath: utest.framework.TestPath) = {
    val repo = "https://github.com/" + testPath.value.last
    val name = repo.split("/").last
    val path = Paths.get("out/repos/" + name)
    if (!Files.exists(path)) {
      println("Cloning " + path)

      new java.lang.ProcessBuilder()
        .command("git", "clone", repo, path.toString)
        .directory(new java.io.File("."))
        .start()
        .waitFor()

      new java.lang.ProcessBuilder()
        .command("git", "checkout", commitHash)
        .directory(path.toFile)
        .start()
        .waitFor()

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
        import sys.process._
        (Seq("python", "pythonparse/test/resources/parse.py", p).!, p)
      }
    }), Duration.Inf).groupBy(_._1).mapValues(_.map(_._2))
    val selfParsed = grouped(0) groupBy { x =>
      print(".")
      parse(new String(Files.readAllBytes(Paths.get(x))), pythonparse.Statements.file_input(_)).getClass
    }

    selfParsed.get(classOf[Parsed.Failure]) match{
      case None => (grouped.mapValues(_.length), selfParsed.mapValues(_.length))
      case Some(xs) => throw new Exception(xs.mkString("\n"))
    }

  }
  val tests = Tests {
    "dropbox/changes" - check("37e23c3141b75e4785cf398d015e3dbca41bdd56")
    "django/django" - check(
      "399a8db33b14a1f707912ac48a185fb0a1204913",
      ignored = Seq("tests/i18n/test_compilation.py")
    )
    "mitsuhiko/flask" - check("9291ead32e2fc8b13cef825186c968944e9ff344")
    "zulip/zulip" - check("b5c107ed27b337ed833ebe9c754889bf078d743e")
    "ansible/ansible"- check("02cd88169764232fd63c776456178fe61d3a214a")
    "kennethreitz/requests" - check("9713289e741960249c94fcb1686746f80e2f20b5")

//    test("test"){
//      val txt = new String(Files.readAllBytes(Paths.get("out/repos/ansible/lib/ansible/modules/cloud/cloudstack/cs_instance.py")))
//      parse(txt).read(pythonparse.Statements.file_input(_))
//    }
//    test("bench"){
//      val path = "pythonparse/jvm/src/test/resources/pythonparse/bench.py"
//      val data = Files.readAllBytes(Paths.get(path))
//      val code = new String(data)
//      import NoWhitespace._
//      parse(code).read(implicit ctx => pythonparse.Statements.file_input ~ End).get
//    }
  }
}
