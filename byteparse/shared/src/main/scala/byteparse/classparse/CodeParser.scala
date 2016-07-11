package byteparse
package classparse

import fastparse.allByte._

object CodeParser {
  sealed abstract class OpCode

  case class AALoad() extends OpCode
  case class AAStore() extends OpCode
  case class AConstNull() extends OpCode
  case class ALoad(index: Int) extends OpCode
  case class ALoad0() extends OpCode
  case class ALoad1() extends OpCode
  case class ALoad2() extends OpCode
  case class ALoad3() extends OpCode
  case class ANewArray(index: Int) extends OpCode
  case class AReturn() extends OpCode
  case class ArrayLength() extends OpCode
  case class AStore(index: Int) extends OpCode
  case class AStore0() extends OpCode
  case class AStore1() extends OpCode
  case class AStore2() extends OpCode
  case class AStore3() extends OpCode
  case class AThrow() extends OpCode

  case class BALoad() extends OpCode
  case class BAStore() extends OpCode
  case class BIPush(byte: Byte) extends OpCode

  case class BreakPoint() extends OpCode

  case class CALoad() extends OpCode
  case class CAStore() extends OpCode

  case class CheckCast(index: Int) extends OpCode

  case class D2F() extends OpCode
  case class D2I() extends OpCode
  case class D2L() extends OpCode
  case class DAdd() extends OpCode
  case class DSub() extends OpCode
  case class DMul() extends OpCode
  case class DNeg() extends OpCode
  case class DDiv() extends OpCode
  case class DRem() extends OpCode

  case class DALoad() extends OpCode
  case class DAStore() extends OpCode
  case class DCmpG() extends OpCode
  case class DCmpL() extends OpCode
  case class DConst0() extends OpCode
  case class DConst1() extends OpCode
  case class DLoad(index: Int) extends OpCode
  case class DLoad0() extends OpCode
  case class DLoad1() extends OpCode
  case class DLoad2() extends OpCode
  case class DLoad3() extends OpCode
  case class DReturn() extends OpCode
  case class DStore(index: Int) extends OpCode
  case class DStore0() extends OpCode
  case class DStore1() extends OpCode
  case class DStore2() extends OpCode
  case class DStore3() extends OpCode

  case class Dup() extends OpCode
  case class Dup2() extends OpCode
  case class DupX1() extends OpCode
  case class DupX2() extends OpCode
  case class Dup2X1() extends OpCode
  case class Dup2X2() extends OpCode

  case class F2D() extends OpCode
  case class F2I() extends OpCode
  case class F2L() extends OpCode
  case class FAdd() extends OpCode
  case class FSub() extends OpCode
  case class FMul() extends OpCode
  case class FNeg() extends OpCode
  case class FDiv() extends OpCode
  case class FRem() extends OpCode

  case class FALoad() extends OpCode
  case class FAStore() extends OpCode
  case class FCmpG() extends OpCode
  case class FCmpL() extends OpCode
  case class FConst0() extends OpCode
  case class FConst1() extends OpCode
  case class FConst2() extends OpCode
  case class FLoad(index: Int) extends OpCode
  case class FLoad0() extends OpCode
  case class FLoad1() extends OpCode
  case class FLoad2() extends OpCode
  case class FLoad3() extends OpCode
  case class FReturn() extends OpCode
  case class FStore(index: Int) extends OpCode
  case class FStore0() extends OpCode
  case class FStore1() extends OpCode
  case class FStore2() extends OpCode
  case class FStore3() extends OpCode

  case class GetField(index: Int) extends OpCode
  case class GetStatic(index: Int) extends OpCode

  case class Goto(offset: Short) extends OpCode
  case class GotoW(offset: Int) extends OpCode

  case class I2B() extends OpCode
  case class I2C() extends OpCode
  case class I2D() extends OpCode
  case class I2F() extends OpCode
  case class I2L() extends OpCode
  case class I2S() extends OpCode
  case class IAdd() extends OpCode
  case class ISub() extends OpCode
  case class IMul() extends OpCode
  case class INeg() extends OpCode
  case class IDiv() extends OpCode
  case class IRem() extends OpCode
  case class IInc(index: Int, const: Int) extends OpCode

  case class IAnd() extends OpCode
  case class IOr() extends OpCode
  case class IShl() extends OpCode
  case class IShr() extends OpCode
  case class IUShr() extends OpCode
  case class IXor() extends OpCode

  case class IALoad() extends OpCode
  case class IAStore() extends OpCode
  case class IConstM1() extends OpCode
  case class IConst0() extends OpCode
  case class IConst1() extends OpCode
  case class IConst2() extends OpCode
  case class IConst3() extends OpCode
  case class IConst4() extends OpCode
  case class IConst5() extends OpCode
  case class ILoad(index: Int) extends OpCode
  case class ILoad0() extends OpCode
  case class ILoad1() extends OpCode
  case class ILoad2() extends OpCode
  case class ILoad3() extends OpCode
  case class IReturn() extends OpCode
  case class IStore(index: Int) extends OpCode
  case class IStore0() extends OpCode
  case class IStore1() extends OpCode
  case class IStore2() extends OpCode
  case class IStore3() extends OpCode


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

  case class ImpDep1() extends OpCode
  case class ImpDep2() extends OpCode

  case class InstanceOf(index: Int) extends OpCode
  case class InvokeDynamic(index: Int) extends OpCode
  case class InvokeInterface(index: Int, count: Int) extends OpCode
  case class InvokeSpecial(index: Int) extends OpCode
  case class InvokeStatic(index: Int) extends OpCode
  case class InvokeVirtual(index: Int) extends OpCode

  case class JSR(offset: Short) extends OpCode
  case class JSRW(offset: Int) extends OpCode

  case class L2D() extends OpCode
  case class L2F() extends OpCode
  case class L2I() extends OpCode
  case class LAdd() extends OpCode
  case class LSub() extends OpCode
  case class LMul() extends OpCode
  case class LNeg() extends OpCode
  case class LDiv() extends OpCode
  case class LRem() extends OpCode

  case class LAnd() extends OpCode
  case class LOr() extends OpCode
  case class LShl() extends OpCode
  case class LShr() extends OpCode
  case class LUShr() extends OpCode
  case class LXor() extends OpCode

  case class LALoad() extends OpCode
  case class LAStore() extends OpCode
  case class LCmp() extends OpCode
  case class LConst0() extends OpCode
  case class LConst1() extends OpCode
  case class LLoad(index: Int) extends OpCode
  case class LLoad0() extends OpCode
  case class LLoad1() extends OpCode
  case class LLoad2() extends OpCode
  case class LLoad3() extends OpCode
  case class LStore(index: Int) extends OpCode
  case class LStore0() extends OpCode
  case class LStore1() extends OpCode
  case class LStore2() extends OpCode
  case class LStore3() extends OpCode
  case class LReturn() extends OpCode

  case class LDC(index: Int) extends OpCode
  case class LDCW(index: Int) extends OpCode
  case class LDC2W(index: Int) extends OpCode

  case class LookUpSwitch(defaultOffset: Int, pairs: Seq[(Int, Int)]) extends OpCode

  case class MonitorEnter() extends OpCode
  case class MonitorExit() extends OpCode

  case class MutliANewArray(index: Int, dimensions: Int) extends OpCode
  case class New(index: Int) extends OpCode
  case class NewArray(atype: Byte) extends OpCode

  case class Nop() extends OpCode
  case class Pop() extends OpCode
  case class Pop2() extends OpCode
  case class Swap() extends OpCode
  case class PutField(index: Int) extends OpCode
  case class PutStatic(index: Int) extends OpCode
  case class Ret(index: Int) extends OpCode
  case class Return() extends OpCode

  case class SALoad() extends OpCode
  case class SAStore() extends OpCode
  case class SIPush(short: Short) extends OpCode

  case class TableSwitch(defaultOffset: Int, low: Int, high: Int, offsets: Seq[Int]) extends OpCode

  case class Wide() extends OpCode // correct behavior for this command hasn't been implemented yet

  import ByteUtils.BE._

  val opCodeParsers = {

    val localIndex = P( AnyByte.! ).map(b => b(0) & 0xff)
    val poolIndex = P( AnyWordI )
    val offsetIndex = P( AnyWordI ).map(i => i.toShort)
    val offsetIndexWide = P( AnyDwordI ).map(i => i.toInt) // TODO It's done because Dword is actually unsigned

    Map[Int, Parser[OpCode]](
      0x32 -> Pass.map(_ => AALoad()),
      0x53 -> Pass.map(_ => AAStore()),
      0x01 -> Pass.map(_ => AConstNull()),
      0x2a -> Pass.map(_ => ALoad0()),
      0x2b -> Pass.map(_ => ALoad1()),
      0x2c -> Pass.map(_ => ALoad2()),
      0x2d -> Pass.map(_ => ALoad3()),
      0xb0 -> Pass.map(_ => AReturn()),
      0xbe -> Pass.map(_ => ArrayLength()),
      0x4b -> Pass.map(_ => AStore0()),
      0x4c -> Pass.map(_ => AStore1()),
      0x4d -> Pass.map(_ => AStore2()),
      0x4e -> Pass.map(_ => AStore3()),
      0xbf -> Pass.map(_ => AThrow()),

      0x33 -> Pass.map(_ => BALoad()),
      0x54 -> Pass.map(_ => BAStore()),
      0xca -> Pass.map(_ => BreakPoint()),

      0x34 -> Pass.map(_ => CALoad()),
      0x55 -> Pass.map(_ => CAStore()),

      0x90 -> Pass.map(_ => D2F()),
      0x8e -> Pass.map(_ => D2I()),
      0x8f -> Pass.map(_ => D2L()),
      0x63 -> Pass.map(_ => DAdd()),
      0x31 -> Pass.map(_ => DALoad()),
      0x52 -> Pass.map(_ => DAStore()),
      0x98 -> Pass.map(_ => DCmpG()),
      0x97 -> Pass.map(_ => DCmpL()),
      0x0e -> Pass.map(_ => DConst0()),
      0x0f -> Pass.map(_ => DConst1()),
      0x6f -> Pass.map(_ => DDiv()),
      0x26 -> Pass.map(_ => DLoad0()),
      0x27 -> Pass.map(_ => DLoad1()),
      0x28 -> Pass.map(_ => DLoad2()),
      0x29 -> Pass.map(_ => DLoad3()),
      0x6b -> Pass.map(_ => DMul()),
      0x77 -> Pass.map(_ => DNeg()),
      0x73 -> Pass.map(_ => DRem()),
      0xaf -> Pass.map(_ => DReturn()),
      0x47 -> Pass.map(_ => DStore0()),
      0x48 -> Pass.map(_ => DStore1()),
      0x49 -> Pass.map(_ => DStore2()),
      0x4a -> Pass.map(_ => DStore3()),
      0x67 -> Pass.map(_ => DSub()),
      0x59 -> Pass.map(_ => Dup()),
      0x5a -> Pass.map(_ => DupX1()),
      0x5b -> Pass.map(_ => DupX2()),
      0x5c -> Pass.map(_ => Dup2()),
      0x5d -> Pass.map(_ => Dup2X1()),
      0x5e -> Pass.map(_ => Dup2X2()),

      0x8d -> Pass.map(_ => F2D()),
      0x8b -> Pass.map(_ => F2I()),
      0x8c -> Pass.map(_ => F2L()),
      0x62 -> Pass.map(_ => FAdd()),
      0x30 -> Pass.map(_ => FALoad()),
      0x51 -> Pass.map(_ => FAStore()),
      0x96 -> Pass.map(_ => FCmpG()),
      0x95 -> Pass.map(_ => FCmpL()),
      0x0b -> Pass.map(_ => FConst0()),
      0x0c -> Pass.map(_ => FConst1()),
      0x0d -> Pass.map(_ => FConst2()),
      0x6e -> Pass.map(_ => FDiv()),
      0x22 -> Pass.map(_ => FLoad0()),
      0x23 -> Pass.map(_ => FLoad1()),
      0x24 -> Pass.map(_ => FLoad2()),
      0x25 -> Pass.map(_ => FLoad3()),
      0x6a -> Pass.map(_ => FMul()),
      0x76 -> Pass.map(_ => FNeg()),
      0x72 -> Pass.map(_ => FRem()),
      0xae -> Pass.map(_ => FReturn()),
      0x43 -> Pass.map(_ => FStore0()),
      0x44 -> Pass.map(_ => FStore1()),
      0x45 -> Pass.map(_ => FStore2()),
      0x46 -> Pass.map(_ => FStore3()),
      0x66 -> Pass.map(_ => FSub()),

      0x91 -> Pass.map(_ => I2B()),
      0x92 -> Pass.map(_ => I2C()),
      0x87 -> Pass.map(_ => I2D()),
      0x86 -> Pass.map(_ => I2F()),
      0x85 -> Pass.map(_ => I2L()),
      0x93 -> Pass.map(_ => I2S()),
      0x60 -> Pass.map(_ => IAdd()),
      0x2e -> Pass.map(_ => IALoad()),
      0x7e -> Pass.map(_ => IAnd()),
      0x4f -> Pass.map(_ => IAStore()),
      0x02 -> Pass.map(_ => IConstM1()),
      0x03 -> Pass.map(_ => IConst0()),
      0x04 -> Pass.map(_ => IConst1()),
      0x05 -> Pass.map(_ => IConst2()),
      0x06 -> Pass.map(_ => IConst3()),
      0x07 -> Pass.map(_ => IConst4()),
      0x08 -> Pass.map(_ => IConst5()),
      0x6c -> Pass.map(_ => IDiv()),
      0x1a -> Pass.map(_ => ILoad0()),
      0x1b -> Pass.map(_ => ILoad1()),
      0x1c -> Pass.map(_ => ILoad2()),
      0x1d -> Pass.map(_ => ILoad3()),
      0xfe -> Pass.map(_ => ImpDep1()),
      0xff -> Pass.map(_ => ImpDep2()),
      0x68 -> Pass.map(_ => IMul()),
      0x74 -> Pass.map(_ => INeg()),
      0x80 -> Pass.map(_ => IOr()),
      0x70 -> Pass.map(_ => IRem()),
      0xac -> Pass.map(_ => IReturn()),
      0x78 -> Pass.map(_ => IShl()),
      0x7a -> Pass.map(_ => IShr()),
      0x3b -> Pass.map(_ => IStore0()),
      0x3c -> Pass.map(_ => IStore1()),
      0x3d -> Pass.map(_ => IStore2()),
      0x3e -> Pass.map(_ => IStore3()),
      0x64 -> Pass.map(_ => ISub()),
      0x7c -> Pass.map(_ => IUShr()),
      0x82 -> Pass.map(_ => IXor()),

      0x8a -> Pass.map(_ => L2D()),
      0x89 -> Pass.map(_ => L2F()),
      0x88 -> Pass.map(_ => L2I()),
      0x61 -> Pass.map(_ => LAdd()),
      0x2f -> Pass.map(_ => LALoad()),
      0x7f -> Pass.map(_ => LAnd()),
      0x50 -> Pass.map(_ => LAStore()),
      0x94 -> Pass.map(_ => LCmp()),
      0x09 -> Pass.map(_ => LConst0()),
      0x0a -> Pass.map(_ => LConst1()),
      0x6d -> Pass.map(_ => LDiv()),
      0x1e -> Pass.map(_ => LLoad0()),
      0x1f -> Pass.map(_ => LLoad1()),
      0x20 -> Pass.map(_ => LLoad2()),
      0x21 -> Pass.map(_ => LLoad3()),
      0x69 -> Pass.map(_ => LMul()),
      0x75 -> Pass.map(_ => LNeg()),
      0x81 -> Pass.map(_ => LOr()),
      0x71 -> Pass.map(_ => LRem()),
      0xad -> Pass.map(_ => LReturn()),
      0x79 -> Pass.map(_ => LShl()),
      0x7b -> Pass.map(_ => LShr()),
      0x3f -> Pass.map(_ => LStore0()),
      0x40 -> Pass.map(_ => LStore1()),
      0x41 -> Pass.map(_ => LStore2()),
      0x42 -> Pass.map(_ => LStore3()),
      0x65 -> Pass.map(_ => LSub()),
      0x7d -> Pass.map(_ => LUShr()),
      0x83 -> Pass.map(_ => LXor()),

      0xc2 -> Pass.map(_ => MonitorEnter()),
      0xc3 -> Pass.map(_ => MonitorExit()),
      0x00 -> Pass.map(_ => Nop()),
      0x57 -> Pass.map(_ => Pop()),
      0x58 -> Pass.map(_ => Pop2()),
      0xb1 -> Pass.map(_ => Return()),
      0x35 -> Pass.map(_ => SALoad()),
      0x56 -> Pass.map(_ => SAStore()),
      0x5f -> Pass.map(_ => Swap()),
      0xc4 -> Pass.map(_ => Wide()),

      0x10 -> P( AnyByte.! ).map(_(0)).map(BIPush),
      0x11 -> P( AnyWordI ).map(i => i.toShort).map(SIPush),
      0xbc -> P( AnyByte.! ).map(_(0)).map(NewArray),
      0x84 -> P( AnyByte.! ~ AnyByte.!).map {
        case (idx: Array[Byte], const: Array[Byte]) => IInc(idx(0) & 0xff, const(0).toInt)
      },
      0xc5 -> P( AnyWordI ~ AnyByte.! ).map {
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

      0xba -> P( AnyWordI ~ BS(0, 0) ).map(InvokeDynamic),
      0xb9 -> P( AnyWordI ~ AnyByte.! ~ BS(0) ).map {
        case (idx: Int, count: Array[Byte]) => InvokeInterface(idx, count(0) & 0xff)
      },

      0xab ->
        P( AnyDwordI /*default_offset*/ ~
           AnyDwordI /*n_pairs*/.flatMap(l =>
             P( (AnyDwordI /*value*/ ~
                 AnyDwordI /*offset*/).rep(exactly=l) ))
         ).map(LookUpSwitch.tupled),
      0xaa ->
        P( AnyDwordI /*default_offset*/ ~
           (AnyDwordI /*low*/ ~ AnyDwordI /*high*/).flatMap {
              case (low: Int, high: Int) =>
                P( AnyDwordI.rep(exactly=high - low + 1) ).map(offs => (low, high, offs))
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
            case b @ (0xab | 0xaa) => P( AnyByte.rep(exactly=(4 - idx % 4) % 4) ~ opCodeParsers(b))
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
