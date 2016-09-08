package classparse

import fastparse.byte._
import BE._
object CodeParser {
  sealed abstract class OpCode

  case object AALoad extends OpCode
  case object AAStore extends OpCode
  case object AConstNull extends OpCode
  case class ALoad(index: Int) extends OpCode
  case object ALoad0 extends OpCode
  case object ALoad1 extends OpCode
  case object ALoad2 extends OpCode
  case object ALoad3 extends OpCode
  case class ANewArray(index: Int) extends OpCode
  case object AReturn extends OpCode
  case object ArrayLength extends OpCode
  case class AStore(index: Int) extends OpCode
  case object AStore0 extends OpCode
  case object AStore1 extends OpCode
  case object AStore2 extends OpCode
  case object AStore3 extends OpCode
  case object AThrow extends OpCode

  case object BALoad extends OpCode
  case object BAStore extends OpCode
  case class BIPush(byte: Byte) extends OpCode

  case object BreakPoint extends OpCode

  case object CALoad extends OpCode
  case object CAStore extends OpCode

  case class CheckCast(index: Int) extends OpCode

  case object D2F extends OpCode
  case object D2I extends OpCode
  case object D2L extends OpCode
  case object DAdd extends OpCode
  case object DSub extends OpCode
  case object DMul extends OpCode
  case object DNeg extends OpCode
  case object DDiv extends OpCode
  case object DRem extends OpCode

  case object DALoad extends OpCode
  case object DAStore extends OpCode
  case object DCmpG extends OpCode
  case object DCmpL extends OpCode
  case object DConst0 extends OpCode
  case object DConst1 extends OpCode
  case class DLoad(index: Int) extends OpCode
  case object DLoad0 extends OpCode
  case object DLoad1 extends OpCode
  case object DLoad2 extends OpCode
  case object DLoad3 extends OpCode
  case object DReturn extends OpCode
  case class DStore(index: Int) extends OpCode
  case object DStore0 extends OpCode
  case object DStore1 extends OpCode
  case object DStore2 extends OpCode
  case object DStore3 extends OpCode

  case object Dup extends OpCode
  case object Dup2 extends OpCode
  case object DupX1 extends OpCode
  case object DupX2 extends OpCode
  case object Dup2X1 extends OpCode
  case object Dup2X2 extends OpCode

  case object F2D extends OpCode
  case object F2I extends OpCode
  case object F2L extends OpCode
  case object FAdd extends OpCode
  case object FSub extends OpCode
  case object FMul extends OpCode
  case object FNeg extends OpCode
  case object FDiv extends OpCode
  case object FRem extends OpCode

  case object FALoad extends OpCode
  case object FAStore extends OpCode
  case object FCmpG extends OpCode
  case object FCmpL extends OpCode
  case object FConst0 extends OpCode
  case object FConst1 extends OpCode
  case object FConst2 extends OpCode
  case class FLoad(index: Int) extends OpCode
  case object FLoad0 extends OpCode
  case object FLoad1 extends OpCode
  case object FLoad2 extends OpCode
  case object FLoad3 extends OpCode
  case object FReturn extends OpCode
  case class FStore(index: Int) extends OpCode
  case object FStore0 extends OpCode
  case object FStore1 extends OpCode
  case object FStore2 extends OpCode
  case object FStore3 extends OpCode

  case class GetField(index: Int) extends OpCode
  case class GetStatic(index: Int) extends OpCode

  case class Goto(offset: Short) extends OpCode
  case class GotoW(offset: Int) extends OpCode

  case object I2B extends OpCode
  case object I2C extends OpCode
  case object I2D extends OpCode
  case object I2F extends OpCode
  case object I2L extends OpCode
  case object I2S extends OpCode
  case object IAdd extends OpCode
  case object ISub extends OpCode
  case object IMul extends OpCode
  case object INeg extends OpCode
  case object IDiv extends OpCode
  case object IRem extends OpCode
  case class IInc(index: Int, const: Int) extends OpCode

  case object IAnd extends OpCode
  case object IOr extends OpCode
  case object IShl extends OpCode
  case object IShr extends OpCode
  case object IUShr extends OpCode
  case object IXor extends OpCode

  case object IALoad extends OpCode
  case object IAStore extends OpCode
  case object IConstM1 extends OpCode
  case object IConst0 extends OpCode
  case object IConst1 extends OpCode
  case object IConst2 extends OpCode
  case object IConst3 extends OpCode
  case object IConst4 extends OpCode
  case object IConst5 extends OpCode
  case class ILoad(index: Int) extends OpCode
  case object ILoad0 extends OpCode
  case object ILoad1 extends OpCode
  case object ILoad2 extends OpCode
  case object ILoad3 extends OpCode
  case object IReturn extends OpCode
  case class IStore(index: Int) extends OpCode
  case object IStore0 extends OpCode
  case object IStore1 extends OpCode
  case object IStore2 extends OpCode
  case object IStore3 extends OpCode


  case class IfACmpEq(offset: Short) extends OpCode
  case class IfACmpNe(offset: Short) extends OpCode

  case class IfICmpEq(offset: Short) extends OpCode
  case class IfICmpGe(offset: Short) extends OpCode
  case class IfICmpGt(offset: Short) extends OpCode
  case class IfICmpLe(offset: Short) extends OpCode
  case class IfICmpLt(offset: Short) extends OpCode
  case class IfICmpNe(offset: Short) extends OpCode

  case class IfEq(offset: Short) extends OpCode
  case class IfGe(offset: Short) extends OpCode
  case class IfGt(offset: Short) extends OpCode
  case class IfLe(offset: Short) extends OpCode
  case class IfLt(offset: Short) extends OpCode
  case class IfNe(offset: Short) extends OpCode
  case class IfNonNull(offset: Short) extends OpCode
  case class IfNull(offset: Short) extends OpCode

  case object ImpDep1 extends OpCode
  case object ImpDep2 extends OpCode

  case class InstanceOf(index: Int) extends OpCode
  case class InvokeDynamic(index: Int) extends OpCode
  case class InvokeInterface(index: Int, count: Int) extends OpCode
  case class InvokeSpecial(index: Int) extends OpCode
  case class InvokeStatic(index: Int) extends OpCode
  case class InvokeVirtual(index: Int) extends OpCode

  case class JSR(offset: Short) extends OpCode
  case class JSRW(offset: Int) extends OpCode

  case object L2D extends OpCode
  case object L2F extends OpCode
  case object L2I extends OpCode
  case object LAdd extends OpCode
  case object LSub extends OpCode
  case object LMul extends OpCode
  case object LNeg extends OpCode
  case object LDiv extends OpCode
  case object LRem extends OpCode

  case object LAnd extends OpCode
  case object LOr extends OpCode
  case object LShl extends OpCode
  case object LShr extends OpCode
  case object LUShr extends OpCode
  case object LXor extends OpCode

  case object LALoad extends OpCode
  case object LAStore extends OpCode
  case object LCmp extends OpCode
  case object LConst0 extends OpCode
  case object LConst1 extends OpCode
  case class LLoad(index: Int) extends OpCode
  case object LLoad0 extends OpCode
  case object LLoad1 extends OpCode
  case object LLoad2 extends OpCode
  case object LLoad3 extends OpCode
  case class LStore(index: Int) extends OpCode
  case object LStore0 extends OpCode
  case object LStore1 extends OpCode
  case object LStore2 extends OpCode
  case object LStore3 extends OpCode
  case object LReturn extends OpCode

  case class LDC(index: Int) extends OpCode
  case class LDCW(index: Int) extends OpCode
  case class LDC2W(index: Int) extends OpCode

  case class LookUpSwitch(defaultOffset: Int, pairs: Seq[(Int, Int)]) extends OpCode

  case object MonitorEnter extends OpCode
  case object MonitorExit extends OpCode

  case class MutliANewArray(index: Int, dimensions: Int) extends OpCode
  case class New(index: Int) extends OpCode
  case class NewArray(atype: Byte) extends OpCode

  case object Nop extends OpCode
  case object Pop extends OpCode
  case object Pop2 extends OpCode
  case object Swap extends OpCode
  case class PutField(index: Int) extends OpCode
  case class PutStatic(index: Int) extends OpCode
  case class Ret(index: Int) extends OpCode
  case object Return extends OpCode

  case object SALoad extends OpCode
  case object SAStore extends OpCode
  case class SIPush(short: Short) extends OpCode

  case class TableSwitch(defaultOffset: Int, low: Int, high: Int, offsets: Seq[Int]) extends OpCode

  case object Wide extends OpCode // correct behavior for this command hasn't been implemented yet



  val opCodeParsers = {

    val localIndex = P( AnyByte.! ).map(b => b(0) & 0xff)
    val poolIndex = P( UInt16 )
    val offsetIndex = P( UInt16 ).map(i => i.toShort)
    val offsetIndexWide = P( UInt32 ).map(i => i.toInt) // TODO It's done because Dword is actually unsigned

    Map[Int, Parser[OpCode]](
      0x32 -> PassWith(CodeParser.AALoad),
      0x53 -> PassWith(AAStore),
      0x01 -> PassWith(AConstNull),
      0x2a -> PassWith(ALoad0),
      0x2b -> PassWith(ALoad1),
      0x2c -> PassWith(ALoad2),
      0x2d -> PassWith(ALoad3),
      0xb0 -> PassWith(AReturn),
      0xbe -> PassWith(ArrayLength),
      0x4b -> PassWith(AStore0),
      0x4c -> PassWith(AStore1),
      0x4d -> PassWith(AStore2),
      0x4e -> PassWith(AStore3),
      0xbf -> PassWith(AThrow),

      0x33 -> PassWith(BALoad),
      0x54 -> PassWith(BAStore),
      0xca -> PassWith(BreakPoint),

      0x34 -> PassWith(CALoad),
      0x55 -> PassWith(CAStore),

      0x90 -> PassWith(D2F),
      0x8e -> PassWith(D2I),
      0x8f -> PassWith(D2L),
      0x63 -> PassWith(DAdd),
      0x31 -> PassWith(DALoad),
      0x52 -> PassWith(DAStore),
      0x98 -> PassWith(DCmpG),
      0x97 -> PassWith(DCmpL),
      0x0e -> PassWith(DConst0),
      0x0f -> PassWith(DConst1),
      0x6f -> PassWith(DDiv),
      0x26 -> PassWith(DLoad0),
      0x27 -> PassWith(DLoad1),
      0x28 -> PassWith(DLoad2),
      0x29 -> PassWith(DLoad3),
      0x6b -> PassWith(DMul),
      0x77 -> PassWith(DNeg),
      0x73 -> PassWith(DRem),
      0xaf -> PassWith(DReturn),
      0x47 -> PassWith(DStore0),
      0x48 -> PassWith(DStore1),
      0x49 -> PassWith(DStore2),
      0x4a -> PassWith(DStore3),
      0x67 -> PassWith(DSub),
      0x59 -> PassWith(Dup),
      0x5a -> PassWith(DupX1),
      0x5b -> PassWith(DupX2),
      0x5c -> PassWith(Dup2),
      0x5d -> PassWith(Dup2X1),
      0x5e -> PassWith(Dup2X2),

      0x8d -> PassWith(F2D),
      0x8b -> PassWith(F2I),
      0x8c -> PassWith(F2L),
      0x62 -> PassWith(FAdd),
      0x30 -> PassWith(FALoad),
      0x51 -> PassWith(FAStore),
      0x96 -> PassWith(FCmpG),
      0x95 -> PassWith(FCmpL),
      0x0b -> PassWith(FConst0),
      0x0c -> PassWith(FConst1),
      0x0d -> PassWith(FConst2),
      0x6e -> PassWith(FDiv),
      0x22 -> PassWith(FLoad0),
      0x23 -> PassWith(FLoad1),
      0x24 -> PassWith(FLoad2),
      0x25 -> PassWith(FLoad3),
      0x6a -> PassWith(FMul),
      0x76 -> PassWith(FNeg),
      0x72 -> PassWith(FRem),
      0xae -> PassWith(FReturn),
      0x43 -> PassWith(FStore0),
      0x44 -> PassWith(FStore1),
      0x45 -> PassWith(FStore2),
      0x46 -> PassWith(FStore3),
      0x66 -> PassWith(FSub),

      0x91 -> PassWith(I2B),
      0x92 -> PassWith(I2C),
      0x87 -> PassWith(I2D),
      0x86 -> PassWith(I2F),
      0x85 -> PassWith(I2L),
      0x93 -> PassWith(I2S),
      0x60 -> PassWith(IAdd),
      0x2e -> PassWith(IALoad),
      0x7e -> PassWith(IAnd),
      0x4f -> PassWith(IAStore),
      0x02 -> PassWith(IConstM1),
      0x03 -> PassWith(IConst0),
      0x04 -> PassWith(IConst1),
      0x05 -> PassWith(IConst2),
      0x06 -> PassWith(IConst3),
      0x07 -> PassWith(IConst4),
      0x08 -> PassWith(IConst5),
      0x6c -> PassWith(IDiv),
      0x1a -> PassWith(ILoad0),
      0x1b -> PassWith(ILoad1),
      0x1c -> PassWith(ILoad2),
      0x1d -> PassWith(ILoad3),
      0xfe -> PassWith(ImpDep1),
      0xff -> PassWith(ImpDep2),
      0x68 -> PassWith(IMul),
      0x74 -> PassWith(INeg),
      0x80 -> PassWith(IOr),
      0x70 -> PassWith(IRem),
      0xac -> PassWith(IReturn),
      0x78 -> PassWith(IShl),
      0x7a -> PassWith(IShr),
      0x3b -> PassWith(IStore0),
      0x3c -> PassWith(IStore1),
      0x3d -> PassWith(IStore2),
      0x3e -> PassWith(IStore3),
      0x64 -> PassWith(ISub),
      0x7c -> PassWith(IUShr),
      0x82 -> PassWith(IXor),

      0x8a -> PassWith(L2D),
      0x89 -> PassWith(L2F),
      0x88 -> PassWith(L2I),
      0x61 -> PassWith(LAdd),
      0x2f -> PassWith(LALoad),
      0x7f -> PassWith(LAnd),
      0x50 -> PassWith(LAStore),
      0x94 -> PassWith(LCmp),
      0x09 -> PassWith(LConst0),
      0x0a -> PassWith(LConst1),
      0x6d -> PassWith(LDiv),
      0x1e -> PassWith(LLoad0),
      0x1f -> PassWith(LLoad1),
      0x20 -> PassWith(LLoad2),
      0x21 -> PassWith(LLoad3),
      0x69 -> PassWith(LMul),
      0x75 -> PassWith(LNeg),
      0x81 -> PassWith(LOr),
      0x71 -> PassWith(LRem),
      0xad -> PassWith(LReturn),
      0x79 -> PassWith(LShl),
      0x7b -> PassWith(LShr),
      0x3f -> PassWith(LStore0),
      0x40 -> PassWith(LStore1),
      0x41 -> PassWith(LStore2),
      0x42 -> PassWith(LStore3),
      0x65 -> PassWith(LSub),
      0x7d -> PassWith(LUShr),
      0x83 -> PassWith(LXor),

      0xc2 -> PassWith(MonitorEnter),
      0xc3 -> PassWith(MonitorExit),
      0x00 -> PassWith(Nop),
      0x57 -> PassWith(Pop),
      0x58 -> PassWith(Pop2),
      0xb1 -> PassWith(Return),
      0x35 -> PassWith(SALoad),
      0x56 -> PassWith(SAStore),
      0x5f -> PassWith(Swap),
      0xc4 -> PassWith(Wide),

      0x10 -> P( AnyByte.! ).map(_(0)).map(BIPush),
      0x11 -> P( UInt16 ).map(i => i.toShort).map(SIPush),
      0xbc -> P( AnyByte.! ).map(_(0)).map(NewArray),
      0x84 -> P( AnyByte.! ~ AnyByte.!).map {
        case (idx: Array[Byte], const: Array[Byte]) => IInc(idx(0) & 0xff, const(0).toInt)
      },
      0xc5 -> P( UInt16 ~ AnyByte.! ).map {
        case (index: Int, dims: Array[Byte]) => MutliANewArray(index, dims(0) & 0xff)
      },

      0x19 -> localIndex.map(ALoad),
      0x3a -> localIndex.map(AStore),
      0x18 -> localIndex.map(DLoad),
      0x39 -> localIndex.map(DStore),
      0x17 -> localIndex.map(FLoad),
      0x38 -> localIndex.map(FStore),
      0x15 -> localIndex.map(ILoad),
      0x36 -> localIndex.map(IStore),
      0x12 -> localIndex.map(LDC),
      0x16 -> localIndex.map(LLoad),
      0x37 -> localIndex.map(LStore),
      0xa9 -> localIndex.map(Ret),

      0xbd -> poolIndex.map(ANewArray),
      0xc0 -> poolIndex.map(CheckCast),
      0xb4 -> poolIndex.map(GetField),
      0xb2 -> poolIndex.map(GetStatic),
      0xc1 -> poolIndex.map(InstanceOf),
      0xb7 -> poolIndex.map(InvokeSpecial),
      0xb8 -> poolIndex.map(InvokeStatic),
      0xb6 -> poolIndex.map(InvokeVirtual),
      0x13 -> poolIndex.map(LDCW),
      0x14 -> poolIndex.map(LDC2W),
      0xbb -> poolIndex.map(New),
      0xb5 -> poolIndex.map(PutField),
      0xb3 -> poolIndex.map(PutStatic),

      0xa7 -> offsetIndex.map(Goto),
      0xa5 -> offsetIndex.map(IfACmpEq),
      0xa6 -> offsetIndex.map(IfACmpNe),
      0x9f -> offsetIndex.map(IfICmpEq),
      0xa2 -> offsetIndex.map(IfICmpGe),
      0xa3 -> offsetIndex.map(IfICmpGt),
      0xa4 -> offsetIndex.map(IfICmpLe),
      0xa1 -> offsetIndex.map(IfICmpLt),
      0xa0 -> offsetIndex.map(IfICmpNe),
      0x99 -> offsetIndex.map(IfEq),
      0x9c -> offsetIndex.map(IfGe),
      0x9d -> offsetIndex.map(IfGt),
      0x9e -> offsetIndex.map(IfLe),
      0x9b -> offsetIndex.map(IfLt),
      0x9a -> offsetIndex.map(IfNe),
      0xc7 -> offsetIndex.map(IfNonNull),
      0xc6 -> offsetIndex.map(IfNull),
      0xa8 -> offsetIndex.map(JSR),

      0xc8 -> offsetIndexWide.map(GotoW),
      0xc9 -> offsetIndexWide.map(JSRW),

      0xba -> P( UInt16 ~ BS(0, 0) ).map(InvokeDynamic),
      0xb9 -> P( UInt16 ~ AnyByte.! ~ BS(0) ).map {
        case (idx: Int, count: Array[Byte]) => InvokeInterface(idx, count(0) & 0xff)
      },

      0xab ->
        P( Int32 /*default_offset*/ ~
           Int32 /*n_pairs*/.flatMap(l =>
             (Int32 /*value*/ ~
              Int32 /*offset*/).rep(exactly=l))
         ).map(LookUpSwitch.tupled),
      0xaa ->
        P( Int32 /*default_offset*/ ~
           (Int32 /*low*/ ~ Int32 /*high*/).flatMap {
              case (low: Int, high: Int) =>
                Int32.rep(exactly=high - low + 1).map(offs => (low, high, offs))
            }
         ).map {
            case (defaultOffset: Int, (low: Int, high: Int, offs: Seq[Int])) =>
              TableSwitch(defaultOffset, low, high, offs)
          }
    )
  }

  def parseCode(code: Array[Byte]): Seq[OpCode] = {
    val opCodes =
      P( (AnyByte.! ~ Index).flatMap {
          case (bs: Array[Byte], idx: Int) => (bs(0) & 0xff) match {
            case b @ (0xab | 0xaa) => AnyBytes((4 - idx % 4) % 4) ~ opCodeParsers(b)
            case b: Int => opCodeParsers(b)
          }
        }.rep
      )

    opCodes.parse(code) match {
      case Parsed.Success(res, _) => res
      case f: Parsed.Failure => Seq()
    }
  }
}
