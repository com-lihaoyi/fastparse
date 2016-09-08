package classparse

import fastparse.byte._
import BE._
import scala.collection.mutable.ArrayBuffer
// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7

object ClassParse {

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

    case class NopInfo() extends PoolInfo // it's used as a stub for Long and Double info
                                          // see https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.4.5

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

    case class ClassFileInfo(minorVersion: Int,           majorVersion: Int,
                             pool: Seq[PoolInfo],         accessFlags: Array[Byte],
                             thisClassIndex: Int,         superClassIndex: Int,
                             interfacesIndexes: Seq[Int], fields: Seq[FieldInfo],
                             methods: Seq[MethodInfo],    attributes: Seq[AttributeInfo]) {

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
    import Info._
    import ClassAttributes.Attribute

    sealed trait PoolItem

    case object Nop extends PoolItem // see NopeInfo

    case class Class(name: String) extends PoolItem

    object Class {
      def apply(classInfo: Info.ClassFileInfo, info: ClassInfo): Class =
        Class(classInfo.getStringByIndex(info.nameIndex))
    }

    case class NameAndType(name: String, descriptor: String) extends PoolItem

    object NameAndType {
      def apply(classInfo: Info.ClassFileInfo, info: NameAndTypeInfo): NameAndType =
        NameAndType(
          classInfo.getStringByIndex(info.nameIndex),
          classInfo.getStringByIndex(info.descriptorIndex)
        )
    }

    sealed trait Ref extends PoolItem
    case class FieldRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class MethodRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class InterfaceMethodRef(name: String, descriptor: String, refClass: Class) extends Ref

    object FieldRef {
      def apply(classInfo: Info.ClassFileInfo, info: FieldRefInfo): FieldRef = {
        val NameAndType(name, descriptor) = NameAndType(
            classInfo,
            classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get
          )

        FieldRef(
          name,
          descriptor,
          Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get)
        )
      }
    }

    object MethodRef {
      def apply(classInfo: Info.ClassFileInfo, info: MethodRefInfo): MethodRef = {
        val NameAndType(name, descriptor) = NameAndType(
          classInfo,
          classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get
        )

        MethodRef(
          name,
          descriptor,
          Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get)
        )
      }
    }

    object InterfaceMethodRef {
      def apply(classInfo: Info.ClassFileInfo, info: InterfaceMethodRefInfo): InterfaceMethodRef = {
        val NameAndType(name, descriptor) = NameAndType(
          classInfo,
          classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get
        )

        InterfaceMethodRef(
          name,
          descriptor,
          Class(classInfo, classInfo.getInfoByIndex[ClassInfo](info.classIndex).get)
        )
      }
    }

    object RefKind extends Enumeration {
      val getField, getStatic, putField, putStatic,
          invokeVirtual, invokeStatic, invokeSpecial, newInvokeSpecial,
          invokeInterface = Value
    }

    case class BasicElemPool(basicElem: BasicElem) extends PoolItem

    object BasicElemPool {
      def apply(classInfo: Info.ClassFileInfo, info: BasicElemInfo) =
        new BasicElemPool(info.basicElem)
    }

    case class MethodHandle(kind: RefKind.Value, ref: Ref) extends PoolItem

    object MethodHandle {
      def apply(classInfo: Info.ClassFileInfo, info: MethodHandleInfo) =
        new MethodHandle(
          RefKind(info.referenceKind - 1),
          classInfo.getInfoByIndex[PoolInfo](info.referenceIndex).get match {
            case fr: FieldRefInfo => FieldRef(classInfo, fr)
            case mr: MethodRefInfo => MethodRef(classInfo, mr)
            case imr: InterfaceMethodRefInfo => InterfaceMethodRef(classInfo, imr)
          }
        )
    }

    case class MethodType(descriptor: String) extends PoolItem

    object MethodType {
      def apply(classInfo: Info.ClassFileInfo, info: MethodTypeInfo) =
        new MethodType(classInfo.getStringByIndex(info.desctiptorIndex))
    }

    case class InvokeDynamic(name: String, descriptor: String, bootstrapIndex: Int) extends PoolItem

    object InvokeDynamic {
      def apply(classInfo: Info.ClassFileInfo, info: InvokeDynamicInfo): InvokeDynamic = {
        val NameAndType(name, descriptor) = NameAndType(
          classInfo,
          classInfo.getInfoByIndex[NameAndTypeInfo](info.nameAndTypeIndex).get
        )

        InvokeDynamic(name, descriptor, info.bootstrapMethodAttrIndex)
      }
    }


    private def getFlag(f: Byte, mask: Int) = (f & mask) != 0

    case class InnerClassFlags(accPublic: Boolean,   accPrivate: Boolean,   accProtected: Boolean,
                               accStatic: Boolean,   accFinal: Boolean,     accInterface: Boolean,
                               accAbstract: Boolean, accSynthetic: Boolean, accAnnotation: Boolean,
                               accEnum: Boolean){

      def this(fs: Array[Byte]) = this(
        getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
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

      def this(fs: Array[Byte]) = this(
        getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
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

      def this(fs: Array[Byte]) = this(
        getFlag(fs(1), 0x01), getFlag(fs(1), 0x02), getFlag(fs(1), 0x04),
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

      def this(fs: Array[Byte]) = this(
        getFlag(fs(1), 0x01), getFlag(fs(1), 0x10), getFlag(fs(1), 0x20),
        getFlag(fs(0), 0x02), getFlag(fs(0), 0x04), getFlag(fs(0), 0x10),
        getFlag(fs(0), 0x20), getFlag(fs(0), 0x40))
    }

    object ClassFlags {
      def apply(fs: Array[Byte]) = new ClassFlags(fs)
    }

    case class Field(name: String, descriptor: String, flags: FieldFlags, attributes: Seq[Attribute])

    case class Method(name: String, descriptor: String, flags: MethodFlags, attributes: Seq[Attribute])

    case class ClassFile(minorVersion: Int,       majorVersion: Int,
                         accessFlags: ClassFlags, pool: Seq[PoolItem],
                         thisClass: Class,        superClass: Option[Class],
                         interfaces: Seq[Class],  fields: Seq[Field],
                         methods: Seq[Method],    attributes: Seq[Attribute])

    def convertToPoolItem(classInfo: Info.ClassFileInfo, info: PoolInfo): PoolItem = {
      info match {
        case info: NopInfo => Nop
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

    def convertToAst(classInfo: Info.ClassFileInfo): ClassFile = {
      import ClassAttributes.convertToAttribute

      def getFieldFromInfo(fieldInfo: FieldInfo) = Field(
        classInfo.getStringByIndex(fieldInfo.nameIndex),
        classInfo.getStringByIndex(fieldInfo.descriptorIndex),
        FieldFlags(fieldInfo.accessFlags),
        fieldInfo.attributes.map(attr => convertToAttribute(classInfo, attr))
      )

      def getMethodFromInfo(methodInfo: MethodInfo) = Method(
        classInfo.getStringByIndex(methodInfo.nameIndex),
        classInfo.getStringByIndex(methodInfo.descriptorIndex),
        MethodFlags(methodInfo.accessFlags),
        methodInfo.attributes.map(attr => convertToAttribute(classInfo, attr))
      )

      ClassFile(
        classInfo.minorVersion,
        classInfo.majorVersion,
        ClassFlags(classInfo.accessFlags),
        classInfo.pool.map(info => convertToPoolItem(classInfo, info)),
        Class(classInfo, classInfo.getInfoByIndex[ClassInfo](classInfo.thisClassIndex).get),
        if (classInfo.superClassIndex == 0)
          None
        else
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


  val constantClassInfo = {
    val name_index = UInt16
    P( BS(7) ~/ name_index ).map(ClassInfo)
  }

  val constantFieldRefInfo = {
    val class_index = UInt16
    val name_and_type_index = UInt16
    P( BS(9) ~/ class_index ~ name_and_type_index ).map(FieldRefInfo.tupled)
  }

  val constantMethodRefInfo = {
    val class_index = UInt16
    val name_and_type_index = UInt16
    P( BS(10) ~/ class_index ~ name_and_type_index ).map(MethodRefInfo.tupled)
  }

  val constantInterfaceMethodRefInfo = {
    val class_index = UInt16
    val name_and_type_index = UInt16
    P( BS(11) ~/ class_index ~ name_and_type_index ).map(InterfaceMethodRefInfo.tupled)
  }

  val constantStringInfo = {
    val string_index = UInt16
    P( BS(8) ~/ string_index ).map(StringInfo)
  }

  val constantIntInfo = P( BS(3) ~/ Int32 ).map(i =>
    BasicElemInfo(IntElem(i))
  )
  val constantFloatInfo = P( BS(4) ~/ Float32 ).map(f =>
    BasicElemInfo(FloatElem(f))
  )
  val constantLongInfo = P( BS(5) ~/ Int64 ).map(i =>
    BasicElemInfo(LongElem(i))
  )
  val constantDoubleInfo = P( BS(6) ~/ Float64 ).map(f =>
    BasicElemInfo(DoubleElem(f))
  )

  val constantNameAndTypeInfo = {
    val name_index = UInt16
    val descriptor_index = UInt16
    P( BS(12) ~/ name_index ~ descriptor_index ).map(NameAndTypeInfo.tupled)
  }

  val constantUtf8Info =
    P( BS(1) ~/ UInt16.flatMap(l => AnyBytes(l).!) ).map(Utf8Info)

  val constantMethodHandleInfo = {
    val reference_kind = AnyByte.!
    val reference_index = UInt16
    P( BS(15) ~/ reference_kind ~ reference_index ).map {
      case (refKind, refIdx) => MethodHandleInfo(refKind(0).toInt, refIdx)
    }
  }

  val constantMethodTypeInfo = {
    val descriptor_index = UInt16
    P( BS(16) ~/ descriptor_index ).map(MethodTypeInfo)
  }

  val constantInvokeDynamicInfo = {
    val bootstrap_method_attr_index = UInt16
    val name_and_type_index = UInt16
    P( BS(18) ~/ bootstrap_method_attr_index ~ name_and_type_index ).map(InvokeDynamicInfo.tupled)
  }

  val singleConstantPoolItem = P( constantClassInfo      | constantFieldRefInfo |
                                  constantMethodRefInfo  | constantInterfaceMethodRefInfo |
                                  constantStringInfo     | constantIntInfo |
                                  constantFloatInfo      | constantNameAndTypeInfo |
                                  constantUtf8Info       | constantMethodHandleInfo |
                                  constantMethodTypeInfo | constantInvokeDynamicInfo )

  val doubleConstantPoolItem = P( constantDoubleInfo | constantLongInfo)

  def constantPool(count: Int): P[Seq[PoolInfo]] =
    if (count == 0) {
      Pass.map(_ => ArrayBuffer[PoolInfo]())
    } else {
        P( singleConstantPoolItem.~/.flatMap(item => constantPool(count - 1).map(_ :+ item)) |
           doubleConstantPoolItem.~/.flatMap(item => constantPool(count - 2).map(_ :+ NopInfo() :+ item )) )
      //TODO it's pretty slow solution, as I think, but it's only one that I know currently
    }

  val attributeInfo = {
    val attribute_name_index = UInt16
    val attributes = Int32.flatMap(l => AnyByte.rep(exactly = l).!)

    P( attribute_name_index ~ attributes ).map(AttributeInfo.tupled)
  }


  val fieldInfo = {
    val access_flags = Word16.!
    val name_index = UInt16
    val descriptor_index = UInt16
    val attributes = repeatWithSize(UInt16, attributeInfo.~/)

    P( access_flags ~ name_index ~ descriptor_index ~ attributes ).map(FieldInfo.tupled)
  }

  val methodInfo = {
    val access_flags = Word16.!
    val name_index = UInt16
    val descriptor_index = UInt16
    val attributes = repeatWithSize(UInt16, attributeInfo.~/)

    P(access_flags ~ name_index ~ descriptor_index ~ attributes).map(MethodInfo.tupled)
  }

  val classFile = {
    val minor_version = UInt16
    val major_version = UInt16
    val constant_pool = UInt16.flatMap(l => constantPool(l - 1).map(_.reverse))
    val access_flags = Word16.!
    val this_class = UInt16
    val super_class = UInt16
    val interfaces = repeatWithSize(UInt16, UInt16.~/)
    val fields = repeatWithSize(UInt16, fieldInfo.~/)
    val methods = repeatWithSize(UInt16, methodInfo.~/)
    val attributes = repeatWithSize(UInt16, attributeInfo.~/)

    P(
      BS(0xCA, 0xFE, 0xBA, 0xBE) ~/
      minor_version ~ major_version ~ constant_pool ~
      access_flags ~  this_class ~    super_class ~
      interfaces ~    fields ~        methods ~       attributes
    ).map(ClassFileInfo.tupled)
  }

}
