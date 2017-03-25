package org.example.parser

import fastparse.all._
import utest._

object MatchTests extends TestSuite{

  val tests = TestSuite{
    'match{
      'failure{
        val p = "A" ~ "B"
        val failed = p.parse("C")
        failed match {
          case Result.Failure(_, index) => assert(index == 0)
          case Result.Success(_, index) => assert(index == -1)
        }
      }
      'success{
        val p = "A" ~ "B"
        val successed = p.parse("AB")
        successed match {
          case Result.Success(_, index) => assert(index == 2)
          case Result.Failure(_, index) => assert(index == -1)
        }
      }
    }
    'typed{
      'failure{
        val p = "A" ~ "B"
        val failed: fastparse.core.Result[_] = p.parse("C")
        failed match {
          case Result.Failure(_, index) => assert(index == 0)
          case Result.Success(_, index) => assert(index == -1)
        }
      }
      'success{
        val p = "A" ~ "B"
        val successed: fastparse.core.Result[_] = p.parse("AB")
        successed match {
          case Result.Success(_, index) => assert(index == 2)
          case Result.Failure(_, index) => assert(index == -1)
        }
      }
    }
  }

}
