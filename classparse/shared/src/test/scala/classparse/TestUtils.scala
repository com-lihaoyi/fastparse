package classparse

import fastparse.byte.all._
import utest._


object TestUtils {

  def TryClass[T](r: => T): Option[T] = {
    try {
      Some(r)
    } catch {
      case error: NoClassDefFoundError => None
      case e: ClassNotFoundException => None
    }
  }

  def checkClassAndClassFile(javaClass: Class[_], classFile: Bytes) = {
    val classInfo = ClassParse.classFile.parse(classFile) match {
      case Parsed.Success(info, _) => Some(info)
      case f: Parsed.Failure => None
    }
    assert(classInfo.isDefined)
    val parsedClass = ClassParse.Ast.convertToAst(classInfo.get)
    assert(parsedClass.thisClass.name.replace('/', '.') == javaClass.getName)
    val classFields = TryClass(javaClass.getDeclaredFields.map(_.getName))
    assert(classFields.isEmpty || parsedClass.fields.map(_.name).toSet == classFields.get.toSet)
    val classMethods = TryClass(javaClass.getDeclaredMethods.map(_.getName))
    assert(classMethods.isEmpty ||
           parsedClass.methods.map(_.name).toSet - "<init>" - "<clinit>" == classMethods.get.toSet)
  }
}
