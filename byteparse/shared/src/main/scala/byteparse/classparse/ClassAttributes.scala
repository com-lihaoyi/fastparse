package byteparse
package classparse

import fastparse.allByte._

object ClassAttributes {
  import ClassParser.Info._
  import ClassParser.BasicElems._
  import ClassParser.Ast._
  import ClassParser._
  import CodeParser._

  sealed abstract class Attribute

  case class BasicAttribute(name: String, info: Array[Byte]) extends Attribute {
    override def equals(other: Any): Boolean =
      other match {
        case attr: BasicAttribute => name == attr.name && info.deep == attr.info.deep
        case _ => false
      }

    override def toString = {
      import fastparse.ElemTypeFormatter.ByteFormatter
      s"BasicAttribute($name,${ByteFormatter.prettyPrint(info)})"
    }
  }

  case class ConstantValueAttribute(value: BasicElem) extends Attribute

  case class ExceptionHandler(startIdx: Int, endIdx: Int, handlerIdx: Int, catchType: Option[Class])

  case class CodeAttribute(maxStack: Int, maxLocals: Int,
                           code: Seq[OpCode], exceptions: Seq[ExceptionHandler],
                           attributes: Seq[Attribute]) extends Attribute

  case class ExceptionsAttribute(exceptions: Seq[Class]) extends Attribute


  case class InnerClass(innerClass: Class, outerClass: Option[Class],
                        innerName: Option[String], accessFlags: InnerClassFlags)

  case class InnerClassesAttribute(classes: Seq[InnerClass]) extends Attribute

  case class EnclosingMethodAttribute(enclosingClass: Class, enclosingMethodName: Option[String],
                                      enclosingMethodDescriptor: Option[String]) extends Attribute

  case class SyntheticAttribute() extends Attribute

  case class SignatureAttribute(signature: String) extends Attribute

  case class SourceFileAttribute(sourceFile: String) extends Attribute

  case class DeprecatedAttribute() extends Attribute

  case class BootstrapMethod(bootstrapMethodRef: MethodHandle, bootstrapArguments: Seq[PoolItem])

  case class BootstrapMethodsAttribute(bootstrapMethods: Seq[BootstrapMethod]) extends Attribute

  import ByteUtils.BE._

  val attributeParsers = Map[String, (ClassFileInfo) => Parser[Attribute]](
    "ConstantValue" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI ).map(idx => ConstantValueAttribute(classInfo.getInfoByIndex[PoolInfo](idx).get match {
          case BasicElemInfo(elem) => elem
          case StringInfo(idx) => StringElem(classInfo.getStringByIndex(idx))
        }))
    },
    "Code" -> {
      (classInfo: ClassFileInfo) => {
        val exceptionHandler =
          P( AnyWordI /*start_pc*/ ~ AnyWordI /*end_pc*/ ~
             AnyWordI /*handler_pc*/ ~ AnyWordI /*catch_type*/
            .map(idx =>
              if (idx == 0) None
              else Some(Class(classInfo, classInfo.getInfoByIndex[ClassInfo](idx).get))) )
            .map(ExceptionHandler.tupled)
        P( AnyWordI /*max_stack*/ ~ AnyWordI /*max_locals*/ ~ AnyDwordI /*code_length*/
          .flatMap(l => P( AnyByte.rep(exactly=l).! ).map(parseCode)) ~ AnyWordI /*exception_table_length*/
          .flatMap(l => P( exceptionHandler.rep(exactly=l) )) ~ AnyWordI /*attributes_count*/
          .flatMap(l => P( attributeInfo.rep(exactly=l) )
          .map(_.map(attr => convertToAttribute(classInfo, attr)))) ).map(CodeAttribute.tupled)
      }
    },
    "Exceptions" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI /*number_of_exceptions*/
          .flatMap(l => P( AnyWordI.rep(exactly=l) ).map(_
          .map(idx => Class(classInfo, classInfo.getInfoByIndex[ClassInfo](idx).get)))) )
          .map(ExceptionsAttribute)
    },
    "InnerClasses" -> {
      (classInfo: ClassFileInfo) =>
        val innerClass =
          P( AnyWordI /*inner_class_info_index*/ ~ AnyWordI /*outer_class_info_index*/ ~
             AnyWordI /*inner_name_index*/ ~       AnyWord.! /*inner_class_access_flags*/ )
           .map {
             case (inIdx: Int, outIdx: Int, inName: Int, inFlags: Array[Byte]) =>
               InnerClass(
                 Class(classInfo, classInfo.getInfoByIndex[ClassInfo](inIdx).get),
                 if (outIdx == 0) None
                 else Some(Class(classInfo, classInfo.getInfoByIndex[ClassInfo](outIdx).get)),
                 if (inName == 0) None
                 else Some(classInfo.getStringByIndex(inName)),
                 InnerClassFlags(inFlags)
               )
           }
        P( AnyWordI /*number_of_classes*/
            .flatMap(l => P( innerClass.rep(exactly=l) ) ) ).map(InnerClassesAttribute)
    },
    "EnclosingMethod" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI /*class_index*/ ~ AnyWordI /*method_index*/).map {
          case (classIdx: Int, methodIdx: Int) =>
            val (name, descriptor) =
              if (methodIdx == 0) (None, None)
              else classInfo.getInfoByIndex[NameAndTypeInfo](methodIdx).get match {
                case NameAndTypeInfo(nameIdx, descriptorIdx) =>
                  (Some(classInfo.getStringByIndex(nameIdx)), Some(classInfo.getStringByIndex(descriptorIdx)))
              }

            EnclosingMethodAttribute(Class(classInfo, classInfo.getInfoByIndex[ClassInfo](classIdx).get),
              name, descriptor)
        }
    },
    "Synthetic" -> {
      (classInfo: ClassFileInfo) => Pass.map(_ => SyntheticAttribute())
    },
    "Signature" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI ).map(idx => SignatureAttribute(classInfo.getStringByIndex(idx)))
    },
    "SourceFile" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI ).map(idx => SourceFileAttribute(classInfo.getStringByIndex(idx)))
    },
    "Deprecated" -> {
      (classInfo: ClassFileInfo) => Pass.map(_ => DeprecatedAttribute())
    },
    "BootstrapMethods" -> {
      (classInfo: ClassFileInfo) => {
        val bootstrapMethod = P( AnyWordI /*bootstrap_method_ref*/ ~ AnyWordI /*num_bootstrap_arguments*/
          .flatMap(l => P( AnyWordI.rep(exactly=l) )) ).map {
          case (refIdx: Int, argsIdxs: Seq[Int]) =>
            BootstrapMethod(MethodHandle(classInfo, classInfo.getInfoByIndex[MethodHandleInfo](refIdx).get),
              argsIdxs.map(argIdx => convertToPoolItem(classInfo, classInfo.getInfoByIndex[PoolInfo](argIdx).get)))
        }
        P( AnyWordI /*num_bootstrap_methods*/.flatMap(l => P( bootstrapMethod.rep(exactly=l) )) )
          .map(BootstrapMethodsAttribute)
      }
    }
  )

  def convertToAttribute(classInfo: Info.ClassFileInfo, attribute: AttributeInfo): Attribute = {
    val attrName = classInfo.getStringByIndex(attribute.nameIndex)
    if (attributeParsers.contains(attrName)) {
      val Parsed.Success(attr, _) = attributeParsers(attrName)(classInfo).parse(attribute.info)
      attr
    } else {
      BasicAttribute(attrName, attribute.info)
    }
  }

}
