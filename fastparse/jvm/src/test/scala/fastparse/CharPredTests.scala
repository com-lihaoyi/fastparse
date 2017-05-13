package fastparse

import fastparse.JsonTests._
import fastparse.core.Parsed
import fastparse.utils.{Generator, Utils}
import utest._

/**
 * Parse the JSON from the parboiled2 testsuite and make
 * sure we can extract meaningful values from it.
 */
object CharPredTests extends TestSuite{
  val tests = TestSuite{
    'hexToInt{
      val hex1 = "0123456789abcdef"
      val ints = Utils.HexUtils.hex2Ints(hex1)
      val hex2 = Utils.HexUtils.ints2Hex(ints)
      assert(hex1 == hex2)
    }

    'charTypes{
      for(c <- Char.MinValue to Char.MaxValue){
        val charPredType = CharPredicates.charTypeLookup(c)
        val jvmType = c.getType
        val index = c.toInt
        assert({implicitly(index); charPredType == jvmType})
      }
    }
  }
}

