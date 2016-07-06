package byteparse

import fastparse.allByte._
import java.nio.ByteBuffer
import java.nio.ByteOrder.{BIG_ENDIAN => BE}

import byteparse.ClassParser.Info._

// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7

object ClassParser {

  object BasicElems {

    sealed abstract class BasicElem

    case class StringElem(string: String) extends BasicElem
    case class IntElem(int: Int) extends BasicElem
    case class FloatElem(float: Float) extends BasicElem
    case class LongElem(long: Long) extends BasicElem
    case class DoubleElem(double: Double) extends BasicElem
  }

  object Info {
    import BasicElems._

    sealed trait PoolInfo

    case class ClassInfo(nameIndex: Int) extends PoolInfo

    case class FieldRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo
    case class MethodRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo
    case class InterfaceMethodRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo

    case class StringInfo(stringIndex: Int) extends PoolInfo

    case class BasicElemInfo(basicElem: BasicElem) extends PoolInfo

    case class NameAndTypeInfo(nameIndex: Int, descriptorIndex: Int) extends PoolInfo

    case class Utf8Info(bytes: Array[Byte]) extends PoolInfo {
      lazy val getString = new String(bytes, "UTF-8")
    }

    case class MethodHandleInfo(referenceKind: Int, referenceIndex: Int) extends PoolInfo

    case class MethodTypeInfo(desctiptorIndex: Int) extends PoolInfo

    case class InvokeDynamicInfo(bootstrapMethodAttrIndex: Int, nameAndTypeIndex: Int) extends PoolInfo

    case class AttributeInfo(nameIndex: Int, info: Array[Byte])

    case class FieldInfo(accessFlags: Array[Byte], nameIndex: Int,
                         descriptorIndex: Int,     attributes: Seq[AttributeInfo])

    case class MethodInfo(accessFlags: Array[Byte], nameIndex: Int,
                          descriptorIndex: Int,     attributes: Seq[AttributeInfo])

    case class ClassFileInfo(minorVersion: Int, majorVersion: Int,
                             pool: Seq[PoolInfo], accessFlags: Array[Byte],
                             thisClassIndex: Int, superClassIndex: Int,
                             interfacesIndexes: Seq[Int], fields: Seq[FieldInfo],
                             methods: Seq[MethodInfo], attributes: Seq[AttributeInfo]) {

      def getInfoByIndex[T](idx: Int): Option[T] = {
        if (idx > pool.length || idx <= 0) {
          None
        } else {
          pool(idx - 1) match {
            case info: T => Some(info)
            case _ => None
          }
        }
      }

      def getStringByIndex(idx: Int) =
        getInfoByIndex[Utf8Info](idx).get.getString

    }

  }

  object Ast {
    import BasicElems._

    sealed trait PoolItem

    case class Class(name: String) extends PoolItem

    object Class {
      def apply(classInfo: Info.ClassFileInfo, info: ClassInfo): Class =
        Class(classInfo.getStringByIndex(info.nameIndex))
    }

    case class NameAndType(name: String, descriptor: String) extends PoolItem

    object NameAndType {
      def apply(classInfo: Info.ClassFileInfo, info: NameAndTypeInfo): NameAndType =
        NameAndType(classInfo.getStringByIndex(info.nameIndex), classInfo.getStringByIndex(info.descriptorIndex))
    }

    sealed trait Ref extends PoolItem
    case class FieldRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class MethodRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class InterfaceMethodRef(name: String, descriptor: String, refClass: Class) extends Ref

    object FieldRef {
      def apply(classInfo: Info.ClassFileInfo, info: FieldRefInfo): FieldRef = {
        val NameAndType(name, descriptor) =
            NameAndType(classInfo, classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get)
        FieldRef(name, descriptor, Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get))
      }
    }

    object MethodRef {
      def apply(classInfo: Info.ClassFileInfo, info: MethodRefInfo): MethodRef = {
        val NameAndType(name, descriptor) =
          NameAndType(classInfo, classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get)
        MethodRef(name, descriptor, Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get))
      }
    }

    object InterfaceMethodRef {
      def apply(classInfo: Info.ClassFileInfo, info: InterfaceMethodRefInfo): InterfaceMethodRef = {
        val NameAndType(name, descriptor) =
          NameAndType(classInfo, classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get)
        InterfaceMethodRef(name, descriptor,
                           Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get))
      }
    }

    object RefKind extends Enumeration {
      val getField, getStatic, putField, putStatic,
          invokeVirtual, invokeStatic, invokeSpecial, newInvokeSpecial,
          invokeInterface = Value
    }

    case class BasicElemPool(basicElem: BasicElem) extends PoolItem

    object BasicElemPool {
      def apply(classInfo: Info.ClassFileInfo, info: BasicElemInfo) = new BasicElemPool(info.basicElem)
    }

    case class MethodHandle(kind: RefKind.Value, ref: Ref) extends PoolItem

    object MethodHandle {
      def apply(classInfo: Info.ClassFileInfo, info: MethodHandleInfo) =
        new MethodHandle(RefKind(info.referenceKind - 1),
          classInfo.getInfoByIndex[Any](info.referenceIndex).get match {
            case fr: FieldRefInfo => FieldRef(classInfo, fr)
            case mr: MethodRefInfo => MethodRef(classInfo, mr)
            case imr: InterfaceMethodRefInfo => InterfaceMethodRef(classInfo, imr)
          })
    }

    case class MethodType(descriptor: String) extends PoolItem

    object MethodType {
      def apply(classInfo: Info.ClassFileInfo, info: MethodTypeInfo) =
        new MethodType(classInfo.getStringByIndex(info.desctiptorIndex))
    }

    case class InvokeDynamic(name: String, descriptor: String, bootstrapIndex: Int) extends PoolItem

    object InvokeDynamic {
      def apply(classInfo: Info.ClassFileInfo, info: InvokeDynamicInfo): InvokeDynamic = {
        val NameAndType(name, descriptor) =
          NameAndType(classInfo, classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get)

        InvokeDynamic(name, descriptor, info.bootstrapMethodAttrIndex)
      }
    }


    private def getFlag(f: Byte, mask: Int) = (f & mask) != 0

    case class InnerClassFlags(accPublic: Boolean,   accPrivate: Boolean,   accProtected: Boolean,
                               accStatic: Boolean,   accFinal: Boolean,     accInterface: Boolean,
                               accAbstract: Boolean, accSynthetic: Boolean, accAnnotation: Boolean,
                               accEnum: Boolean){

      def this(fs: Array[Byte]) = this(getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
                                       getFlag(fs(1), 0x08), getFlag(fs(1), 0x10), getFlag(fs(0), 0x02),
                                       getFlag(fs(0), 0x04), getFlag(fs(0), 0x10), getFlag(fs(0), 0x20),
                                       getFlag(fs(0), 0x40))
    }


    object InnerClassFlags {
      def apply(fs: Array[Byte]) = new InnerClassFlags(fs)
    }

    case class FieldFlags(accPublic: Boolean,    accPrivate: Boolean,   accProtected: Boolean,
                          accStatic: Boolean,    accFinal: Boolean,     accVolatile: Boolean,
                          accTransient: Boolean, accSynthetic: Boolean, accEnum: Boolean) {

      def this(fs: Array[Byte]) = this(getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
                                       getFlag(fs(1), 0x08), getFlag(fs(1), 0x10), getFlag(fs(1), 0x40),
                                       getFlag(fs(1), 0x80), getFlag(fs(0), 0x10), getFlag(fs(0), 0x40))
    }

    object FieldFlags {
      def apply(fs: Array[Byte]) = new FieldFlags(fs)
    }

    case class MethodFlags(accPublic: Boolean,    accPrivate: Boolean,   accProtected: Boolean,
                           accStatic: Boolean,    accFinal: Boolean,     accSynchronized: Boolean,
                           accBridge: Boolean,    accVarargs: Boolean,   accNative: Boolean,
                           accAbstract: Boolean,  accStrict: Boolean,    accSynthetic: Boolean) {

      def this(fs: Array[Byte]) = this(getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
                                       getFlag(fs(1), 0x08), getFlag(fs(1), 0x10), getFlag(fs(1), 0x20),
                                       getFlag(fs(1), 0x40), getFlag(fs(1), 0x80), getFlag(fs(0), 0x01),
                                       getFlag(fs(0), 0x04), getFlag(fs(0), 0x08), getFlag(fs(0), 0x10))
    }

    object MethodFlags {
      def apply(fs: Array[Byte]) = new MethodFlags(fs)
    }

    case class ClassFlags(accPublic: Boolean,     accFinal: Boolean,    accSuper: Boolean,
                          accInterface: Boolean,  accAbstract: Boolean, accSynthetic: Boolean,
                          accAnnotation: Boolean, accEnum: Boolean){

      def this(fs: Array[Byte]) = this(getFlag(fs(1), 0x01), getFlag(fs(1), 0x10), getFlag(fs(1), 0x20),
                                       getFlag(fs(0), 0x02), getFlag(fs(0), 0x04), getFlag(fs(0), 0x10),
                                       getFlag(fs(0), 0x20), getFlag(fs(0), 0x40))
    }

    object ClassFlags {
      def apply(fs: Array[Byte]) = new ClassFlags(fs)
    }


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
                             code: Array[Byte], exceptions: Seq[ExceptionHandler],
                             attributes: Seq[Attribute]) extends Attribute {
      override def equals(other: Any): Boolean =
        other match {
          case attr: CodeAttribute => maxStack == attr.maxStack && maxLocals == attr.maxLocals &&
            code.deep == attr.code.deep && exceptions == attr.exceptions && attributes == attr.attributes
          case _ => false
        }
    }

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


    case class Field(name: String, descriptor: String, flags: FieldFlags, attributes: Seq[Attribute])

    case class Method(name: String, descriptor: String, flags: MethodFlags, attributes: Seq[Attribute])

    case class ClassFile(minorVersion: Int,       majorVersion: Int,
                         accessFlags: ClassFlags, pool: Seq[PoolItem],
                         thisClass: Class,        superClass: Option[Class],
                         interfaces: Seq[Class],  fields: Seq[Field],
                         methods: Seq[Method],    attributes: Seq[Attribute])

    def convertToPoolItem(classInfo: Info.ClassFileInfo, info: PoolInfo): PoolItem = {
      info match {
        case info: ClassInfo => Class(classInfo, info)
        case info: FieldRefInfo => FieldRef(classInfo, info)
        case info: MethodRefInfo => MethodRef(classInfo, info)
        case info: InterfaceMethodRefInfo => InterfaceMethodRef(classInfo, info)
        case info: MethodTypeInfo => MethodType(classInfo, info)
        case info: NameAndTypeInfo => NameAndType(classInfo, info)
        case StringInfo(strIdx) => BasicElemPool(StringElem(classInfo.getStringByIndex(strIdx)))
        case info: BasicElemInfo => BasicElemPool(classInfo, info)
        case utf8info: Utf8Info => BasicElemPool(StringElem(utf8info.getString))
        case info: MethodHandleInfo => MethodHandle(classInfo, info)
        case info: InvokeDynamicInfo => InvokeDynamic(classInfo, info)
      }
    }

    def convertToAttribute(classInfo: Info.ClassFileInfo, attribute: AttributeInfo): Attribute = {
      val attrName = classInfo.getStringByIndex(attribute.nameIndex)
      if (attributeParsers.contains(attrName)) {
        val Parsed.Success(attr, _) = attributeParsers(attrName)(classInfo).parse(attribute.info)
        attr
      } else {
        BasicAttribute(attrName, attribute.info)
      }
    }


    def convertToAst(classInfo: Info.ClassFileInfo): ClassFile = {

      def getFieldFromInfo(fieldInfo: FieldInfo) =
        Field(classInfo.getStringByIndex(fieldInfo.nameIndex),
              classInfo.getStringByIndex(fieldInfo.descriptorIndex),
              FieldFlags(fieldInfo.accessFlags),
              fieldInfo.attributes.map(attr => convertToAttribute(classInfo, attr)))

      def getMethodFromInfo(methodInfo: MethodInfo) =
        Method(classInfo.getStringByIndex(methodInfo.nameIndex),
               classInfo.getStringByIndex(methodInfo.descriptorIndex),
               MethodFlags(methodInfo.accessFlags),
               methodInfo.attributes.map(attr => convertToAttribute(classInfo, attr)))

      ClassFile(classInfo.minorVersion, classInfo.majorVersion,
        ClassFlags(classInfo.accessFlags),
        classInfo.pool.map(info => convertToPoolItem(classInfo, info)),
        Class(classInfo, classInfo.getInfoByIndex[ClassInfo](classInfo.thisClassIndex).get),
        if (classInfo.superClassIndex == 0) None else
          Some(Class(classInfo, classInfo.getInfoByIndex[ClassInfo](classInfo.superClassIndex).get)),
        classInfo.interfacesIndexes.map(idx =>
          Class(classInfo, classInfo.getInfoByIndex[ClassInfo](idx).get)),
        classInfo.fields.map(getFieldFromInfo),
        classInfo.methods.map(getMethodFromInfo),
        classInfo.attributes.map(attr => convertToAttribute(classInfo, attr))
      )

    }
  }

  import Info._
  import BasicElems._
  import Ast._

  // Big Endian format
  def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(BE)

  val AnyWordI = P( AnyWord.! ).map(wrapByteBuffer(_).getShort.toInt)
  val AnyDwordI = P( AnyDword.! ).map(wrapByteBuffer(_).getInt)

  val constantClassInfo = P( BS(7) ~ AnyWordI /*name_index*/).map(ClassInfo)

  val constantFieldRefInfo = P( BS(9) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(FieldRefInfo.tupled)
  val constantMethodRefInfo = P( BS(10) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(MethodRefInfo.tupled)
  val constantInterfaceMethodRefInfo = P( BS(11) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(InterfaceMethodRefInfo.tupled)

  val constantStringInfo = P( BS(8) ~ AnyWordI /*string_index*/).map(StringInfo)

  val constantIntInfo = P( BS(3) ~ AnyDword.! ).
    map(bs => BasicElemInfo(IntElem(wrapByteBuffer(bs).getInt)))
  val constantFloatInfo = P( BS(4) ~ AnyDword.! ).
    map(bs => BasicElemInfo(FloatElem(wrapByteBuffer(bs).getFloat)))
  val constantLongInfo = P( BS(5) ~ AnyByte.rep(exactly=8).! ).
    map(bs => BasicElemInfo(LongElem(wrapByteBuffer(bs).getLong)))
  val constantDoubleInfo = P( BS(6) ~ AnyByte.rep(exactly=8).! ).
    map(bs => BasicElemInfo(DoubleElem(wrapByteBuffer(bs).getDouble)))

  val constantNameAndTypeInfo = P( BS(12) ~ AnyWordI /*name_index*/ ~ AnyWordI /*descriptor_index*/).
    map(NameAndTypeInfo.tupled)

  val constantUtf8Info = P( BS(1) ~ AnyWordI.flatMap(l => P( AnyByte.rep(exactly=l).! )) ).map(Utf8Info)

  val constantMethodHandleInfo = P( BS(15) ~ AnyByte.! /*reference_kind*/ ~ AnyWordI /*reference_index*/ ).map{
    case (refKind, refIdx) => MethodHandleInfo(refKind(0).toInt, refIdx)
  }

  val constantMethodTypeInfo = P( BS(16) ~ AnyWordI /*descriptor_index*/ ).map(MethodTypeInfo)

  val constantInvokeDynamicInfo = P( BS(18) ~ AnyWordI /*bootstrap_method_attr_index*/ ~
                                     AnyWordI /*name_and_type_index*/ ).map(InvokeDynamicInfo.tupled)

  val constantPoolItem = P( constantClassInfo      | constantFieldRefInfo |
                            constantMethodRefInfo  | constantInterfaceMethodRefInfo |
                            constantStringInfo     | constantIntInfo | constantFloatInfo |
                            constantDoubleInfo     | constantNameAndTypeInfo |
                            constantUtf8Info       | constantMethodHandleInfo |
                            constantMethodTypeInfo | constantInvokeDynamicInfo )

  private val attributeParsers = Map[String, (ClassFileInfo) => Parser[Attribute]](
    "ConstantValue" -> {
      (classInfo: ClassFileInfo) =>
        P( AnyWordI ).map(idx => ConstantValueAttribute(classInfo.getInfoByIndex[PoolInfo](idx).get match {
          case BasicElemInfo(elem) => elem
          case StringInfo(idx) => StringElem(classInfo.getStringByIndex(idx))
        }))
    },
    "Code" -> {
      (classInfo: ClassFileInfo) => {
        val exceptionHandler = P( AnyWordI /*start_pc*/ ~ AnyWordI /*end_pc*/ ~
                                  AnyWordI /*handler_pc*/ ~ AnyWordI /*catch_type*/
            .map(idx =>
              if (idx == 0) None
              else Some(Class(classInfo, classInfo.getInfoByIndex[ClassInfo](idx).get))) )
            .map(ExceptionHandler.tupled)
        P( AnyWordI /*max_stack*/ ~ AnyWordI /*max_locals*/ ~ AnyDwordI /*code_length*/
          .flatMap(l => P( AnyByte.rep(exactly=l).! )) ~ AnyWordI /*exception_table_length*/
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
        P( AnyWordI /*number_of_exceptions*/
          .flatMap(l => P( AnyWordI.rep(exactly=l) ).map(_
          .map(idx => Class(classInfo, classInfo.getInfoByIndex[ClassInfo](idx).get)))) )
          .map(ExceptionsAttribute)
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

  val attributeInfo = P( AnyWordI /*attribute_name_index*/ ~ AnyDwordI /*attribute_length*/
    .flatMap(l => P( AnyByte.rep(exactly=l).! )) ).map(AttributeInfo.tupled)


  val fieldInfo = P( AnyWord.! /*access_flags*/ ~ AnyWordI /*name_index*/ ~
                     AnyWordI /*descriptor_index*/ ~ AnyWordI /*attributes_count*/ .flatMap(l =>
                      P( attributeInfo.rep(exactly=l) )) ).map(FieldInfo.tupled)

  val methodInfo = P( AnyWord.! /*access_flags*/ ~ AnyWordI /*name_index*/ ~
                      AnyWordI /*descriptor_index*/ ~ AnyWordI /*attributes_count*/ .flatMap(l =>
                        P( attributeInfo.rep(exactly=l) )) ).map(MethodInfo.tupled)

  val classFile = P( BS(0xCA, 0xFE, 0xBA, 0xBE) ~
                     AnyWordI /*minor_version*/     ~ AnyWordI /*major_version*/ ~
                     AnyWordI /*constant_pool_count*/.flatMap(l => P( constantPoolItem.rep(exactly=l - 1) )) ~
                     AnyWord.! /*access_flags*/ ~
                     AnyWordI /*this_class*/        ~ AnyWordI /*super_class*/ ~
                     AnyWordI /*interfaces_count*/   .flatMap(l => P( AnyWordI.rep(exactly=l) )) ~
                     AnyWordI /*fields_count*/       .flatMap(l => P( fieldInfo.rep(exactly=l) )) ~
                     AnyWordI /*methods_count*/      .flatMap(l => P( methodInfo.rep(exactly=l) )) ~
                     AnyWordI /*attributes_count*/   .flatMap(l => P( attributeInfo.rep(exactly=l))) ).
    map(ClassFileInfo.tupled)

}
