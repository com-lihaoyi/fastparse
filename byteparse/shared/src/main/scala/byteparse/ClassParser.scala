package byteparse

import fastparse.allByte._
import java.nio.ByteBuffer
import java.nio.ByteOrder.{BIG_ENDIAN => BE}

import byteparse.ClassParser.Info._

// https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html#jvms-4.7

object ClassParser {

  object Info {
    abstract class PoolInfo

    case class ClassInfo(nameIndex: Int) extends PoolInfo

    case class FieldRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo
    case class MethodRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo
    case class InterfaceMethodRefInfo(classIndex: Int, nameAndTypeIndex: Int) extends PoolInfo

    case class StringInfo(stringIndex: Int) extends PoolInfo

    case class IntInfo(int: Int) extends PoolInfo
    case class FloatInfo(float: Float) extends PoolInfo
    case class LongInfo(long: Long) extends PoolInfo
    case class DoubleInfo(double: Double) extends PoolInfo

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

    }

  }

  object Ast {
    case class Class(name: String)

    abstract class Ref
    case class FieldRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class MethodRef(name: String, descriptor: String, refClass: Class) extends Ref
    case class InterfaceMethodRef(name: String, descriptor: String, refClass: Class) extends Ref

    object RefKind extends Enumeration {
      val getField, getStatic, putField, putStatic,
          invokeVirtual, invokeStatic, invokeSpecial, newInvokeSpecial,
          invokeInterface = Value
    }

    case class MethodHandle(kind: RefKind.Value, ref: Ref)

    case class MethodType(descriptor: String)

    case class ConstantInvokeDynamicInfo(name: String, descriptor: String, bootstrapIndex: Int)

    case class Attribute(name: String, info: Array[Byte]) {
      override def equals(other: Any): Boolean =
        other match {
          case attr: Attribute => name == attr.name && info.deep == attr.info.deep
          case _ => false
        }
    }


    private def getFlag(f: Byte, mask: Int) = (f & mask) != 0

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


    case class Field(name: String, descriptor: String, flags: FieldFlags, attributes: Seq[Attribute])

    case class Method(name: String, descriptor: String, flags: MethodFlags, attributes: Seq[Attribute])

    case class ClassFile(minorVersion: Int,       majorVersion: Int,
                         accessFlags: ClassFlags,
                         thisClass: Class,        superClass: Class,
                         interfaces: Seq[Class],  fields: Seq[Field],
                         methods: Seq[Method],    attributes: Seq[Attribute])


    def convertToAst(classFileInfo: Info.ClassFileInfo): ClassFile = {
      def getStringByIndex(idx: Int) = classFileInfo.getInfoByIndex[Utf8Info](idx).get.getString

      def getClassByIndex(idx: Int) = classFileInfo.getInfoByIndex[ClassInfo](idx).get

      def getClassFromInfo(classInfo: ClassInfo) = Class(getStringByIndex(classInfo.nameIndex))

      def getAttributeFromInfo(attributeInfo: AttributeInfo) =
        Attribute(getStringByIndex(attributeInfo.nameIndex), attributeInfo.info)

      def getFieldFromInfo(fieldInfo: FieldInfo) =
        Field(getStringByIndex(fieldInfo.nameIndex),
              getStringByIndex(fieldInfo.descriptorIndex),
              FieldFlags(fieldInfo.accessFlags),
              fieldInfo.attributes.map(getAttributeFromInfo))

      def getMethodFromInfo(methodInfo: MethodInfo) =
        Method(getStringByIndex(methodInfo.nameIndex),
               getStringByIndex(methodInfo.descriptorIndex),
               MethodFlags(methodInfo.accessFlags),
               methodInfo.attributes.map(getAttributeFromInfo))

      ClassFile(classFileInfo.minorVersion, classFileInfo.majorVersion,
        ClassFlags(classFileInfo.accessFlags),
        getClassFromInfo(getClassByIndex(classFileInfo.thisClassIndex)),
        getClassFromInfo(getClassByIndex(classFileInfo.superClassIndex)),
        classFileInfo.interfacesIndexes.map(idx => getClassFromInfo(getClassByIndex(idx))),
        classFileInfo.fields.map(getFieldFromInfo),
        classFileInfo.methods.map(getMethodFromInfo),
        classFileInfo.attributes.map(getAttributeFromInfo)
      )

    }
  }

  // Big Endian format
  def wrapByteBuffer(byteSeq: ByteSeq): ByteBuffer = ByteBuffer.wrap(byteSeq).order(BE)

  val AnyWordI = P( AnyWord.! ).map(wrapByteBuffer(_).getShort.toInt)


  val constantClassInfo = P( BS(7) ~ AnyWordI /*name_index*/).map(Info.ClassInfo)

  val constantFieldRefInfo = P( BS(9) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(Info.FieldRefInfo.tupled)
  val constantMethodRefInfo = P( BS(10) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(Info.MethodRefInfo.tupled)
  val constantInterfaceMethodRefInfo = P( BS(11) ~ AnyWordI /*class_index*/ ~ AnyWordI /*name_and_type_index*/).
    map(Info.InterfaceMethodRefInfo.tupled)

  val constantStringInfo = P( BS(8) ~ AnyWordI /*string_index*/).map(Info.StringInfo)

  val constantIntInfo = P( BS(3) ~ AnyDword.! ).
    map(bs => Info.IntInfo(wrapByteBuffer(bs).getInt))
  val constantFloatInfo = P( BS(4) ~ AnyDword.! ).
    map(bs => Info.FloatInfo(wrapByteBuffer(bs).getFloat))
  val constantLongInfo = P( BS(5) ~ AnyByte.rep(exactly=8).! ).
    map(bs => Info.LongInfo(wrapByteBuffer(bs).getLong))
  val constantDoubleInfo = P( BS(6) ~ AnyByte.rep(exactly=8).! ).
    map(bs => Info.DoubleInfo(wrapByteBuffer(bs).getDouble))

  val constantNameAndTypeInfo = P( BS(12) ~ AnyWordI /*name_index*/ ~ AnyWordI /*descriptor_index*/).
    map(Info.NameAndTypeInfo.tupled)

  val constantUtf8Info = P( BS(1) ~ AnyWordI.flatMap(l => P( AnyByte.rep(exactly=l).! )) ).map(Info.Utf8Info)

  val constantMethodHandleInfo = P( BS(15) ~ AnyByte.! /*reference_kind*/ ~ AnyWordI /*reference_index*/ ).map{
    case (refKind, refIdx) => Info.MethodHandleInfo(refKind(0).toInt, refIdx)
  }

  val constantMethodTypeInfo = P( BS(16) ~ AnyWordI /*descriptor_index*/ ).map(Info.MethodTypeInfo)

  val constantInvokeDynamicInfo = P( BS(18) ~ AnyWordI /*bootstrap_method_attr_index*/ ~
                                     AnyWordI /*name_and_type_index*/ ).map(Info.InvokeDynamicInfo.tupled)

  val constantPoolItem = P( constantClassInfo      | constantFieldRefInfo |
                            constantMethodRefInfo  | constantInterfaceMethodRefInfo |
                            constantStringInfo     | constantIntInfo | constantFloatInfo |
                            constantDoubleInfo     | constantNameAndTypeInfo |
                            constantUtf8Info       | constantMethodHandleInfo |
                            constantMethodTypeInfo | constantInvokeDynamicInfo )

  val attributeInfo = P( AnyWordI /*attribute_name_index*/ ~ AnyDword.! /*attribute_length*/.flatMap(dword => {
    val l = wrapByteBuffer(dword).getInt
    P( AnyByte.rep(exactly=l).! )
  })).map(Info.AttributeInfo.tupled)


  val fieldInfo = P( AnyWord.! /*access_flags*/ ~ AnyWordI /*name_index*/ ~
                     AnyWordI /*descriptor_index*/ ~ AnyWordI /*attributes_count*/ .flatMap(l =>
                      P( attributeInfo.rep(exactly=l) )) ).map(Info.FieldInfo.tupled)

  val methodInfo = P( AnyWord.! /*access_flags*/ ~ AnyWordI /*name_index*/ ~
                      AnyWordI /*descriptor_index*/ ~ AnyWordI /*attributes_count*/ .flatMap(l =>
                        P( attributeInfo.rep(exactly=l) )) ).map(Info.MethodInfo.tupled)

  val classFile = P( BS(0xCA, 0xFE, 0xBA, 0xBE) ~
                     AnyWordI /*minor_version*/     ~ AnyWordI /*major_version*/ ~
                     AnyWordI /*constant_pool_count*/.flatMap(l => P( constantPoolItem.rep(exactly=l - 1) )) ~
                     AnyWord.! /*access_flags*/ ~
                     AnyWordI /*this_class*/        ~ AnyWordI /*super_class*/ ~
                     AnyWordI /*interfaces_count*/   .flatMap(l => P( AnyWordI.rep(exactly=l) )) ~
                     AnyWordI /*fields_count*/       .flatMap(l => P( fieldInfo.rep(exactly=l) )) ~
                     AnyWordI /*methods_count*/      .flatMap(l => P( methodInfo.rep(exactly=l) )) ~
                     AnyWordI /*attributes_count*/   .flatMap(l => P( attributeInfo.rep(exactly=l))) ).
    map(Info.ClassFileInfo.tupled)

}
