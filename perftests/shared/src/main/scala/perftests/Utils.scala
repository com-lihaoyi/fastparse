package perftests

import fastparse.utils.IteratorParserInput
import fastparse.core.{Parsed, Parser}
import fastparse.utils.{IteratorParserInput, ReprOps}

import scala.collection.mutable
import scala.reflect.ClassTag

object Utils {

  def time(f: () => Any, maxTime: Int = 10000): Int = {
    val start = System.currentTimeMillis()
    var count = 0
    while(System.currentTimeMillis() - start < maxTime){
      f()
      count += 1
    }
    count
  }

  def benchmark(name: String, fs: Seq[() => Any], iterations: Int = 5, maxTime: Int = 10000): Seq[Seq[Int]] = {
    println(name)
    println(s"Max time - $maxTime ms. Iterations - $iterations.")
    (1 to iterations).map(i => {
      println(s"Iteration $i")
      fs.zipWithIndex.map(fi => {
        print(s"Benchmark ${fi._2}.")
        val res = time(fi._1, maxTime)
        println(s" Result: $res")
        res
      })
    })
  }

  def benchmarkIteratorBufferSizes[Elem, Repr](parser: Parser[_, Elem, Repr],
                                                   sizes: Seq[Int],
                                                   iteratorFactory: Int => Iterator[Repr])
                                                  (implicit repr: ReprOps[Elem, Repr],
                                                   ct: ClassTag[Elem]): Unit = {

    class LoggedMaxBufferLengthParserInput(data: Iterator[Repr])
      extends IteratorParserInput[Elem, Repr](data) {

      var maxInnerLength = 0

      override def dropBuffer(index: Int): Unit = {
        maxInnerLength = math.max(maxInnerLength, this.innerLength)
        super.dropBuffer(index)
      }
    }

    class LoggedDistributionBufferLengthParserInput(data: Iterator[Repr])
      extends IteratorParserInput[Elem, Repr](data) {

      val drops = mutable.Map.empty[Int, Int].withDefaultValue(0)

      override def dropBuffer(index: Int): Unit = {
        drops(this.innerLength) = drops(this.innerLength) + 1
        super.dropBuffer(index)
      }
    }


    sizes.foreach(s => {
      println("Parsing for batch size " + s)
      val input = new LoggedMaxBufferLengthParserInput(iteratorFactory(s))
      parser.parseInput(input)
      println(s"Batch size: $s. Max buffer size: ${input.maxInnerLength}.")
    })

    val input = new LoggedDistributionBufferLengthParserInput(iteratorFactory(1))
    parser.parseInput(input)
    println("Distibutions of buffer size:")

    val chunkSize = (input.drops.size - 11) / 10
    val lengths = input.drops.toSeq.sorted

    println(lengths.take(11).map(v => s"${v._1}: ${v._2}").mkString("\n"))
    if(lengths.length > 11) {
      println(lengths.drop(11).grouped(chunkSize).toList
        .map(chunk => (chunk.map(_._1).min, chunk.map(_._1).max, chunk.map(_._2).sum))
        .map(v => s"${v._1}-${v._2}: ${v._3}").mkString("\n"))
    }
  }

  def benchmarkAll[Elem, Repr](name: String,
                                   parser: Parser[_, Elem, Repr],
                                   data: Repr, dataFailOpt: Option[Repr],
                                   iteratorFactory: Int => Iterator[Repr])
                                  (implicit repr: ReprOps[Elem, Repr],
                                   ct: ClassTag[Elem]): Unit = {

    val results = Utils.benchmark(s"$name Benchmark",
      Seq(
        Some(() => parser.parse(data)),
        dataFailOpt.map(dataFail =>
          () => parser.parse(dataFail).asInstanceOf[Parsed.Failure[Elem, Repr]].extra.traced
        )
      ).flatten
    )
    println(results.map(_.mkString(" ")).mkString("\n"))

    val sizes = Seq(1, 2, 4, 16, 64, 1024, 4096)
    Utils.benchmarkIteratorBufferSizes(parser, sizes, iteratorFactory)

    val iteratorResults = Utils.benchmark(s"$name Iterator Benchmark",
      sizes.map(s => () => parser.parseIterator(iteratorFactory(s)))
    )

    println(iteratorResults.map(_.mkString(" ")).mkString("\n"))
  }
}


