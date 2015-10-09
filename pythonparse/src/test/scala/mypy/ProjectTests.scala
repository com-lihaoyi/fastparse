package mypy

import utest._
import ammonite.ops._
import ammonite.ops.ImplicitWd._
/**
 * Load external Python code and force feed it through the parser
 * to find out where it blows up
 */
object ProjectTests extends TestSuite{

  def check(repo: Path) = {
    val pythonFiles = ls.rec ! repo |? (_.ext == "py")
    val grouped = pythonFiles.groupBy((% python(cwd / 'src / 'test / 'resources / 'mypy / "parse.py", _)))
    val selfParsed = grouped(0) groupBy { x =>
      println(x)
      Statements.file_input.parse(read ! x).getClass
    }
    selfParsed.get(classOf[fastparse.core.Result.Failure]).map(_.mkString("\n"))
  }
  val tests = TestSuite{
    'changes - check(cwd/'repos/'changes)
    'django - check(cwd/'repos/'django)
    'flask - check(cwd/'repos/'flask)
    'zulip- check(cwd/'repos/'zulip)
    'ansible- check(cwd/'repos/'ansible)
    'requests - check(cwd/'repos/'requests)
    // https://github.com/dropbox/changes
    // https://github.com/django/django
    // https://github.com/mitsuhiko/flask
    // https://github.com/zulip/zulip
    // https://github.com/ansible/ansible
    // https://github.com/kennethreitz/requests
  }
}