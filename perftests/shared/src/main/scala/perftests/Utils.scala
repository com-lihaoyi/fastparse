package perftests

import fastparse.{ElemTypeFormatter, IteratorParserInput}
import fastparse.core.Parser

import scala.collection.mutable

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

  def benchmarkIteratorBufferSizes[ElemType](parser: Parser[_, ElemType, _],
                                             sizes: Seq[Int],
                                             iteratorFactory: Int => Iterator[IndexedSeq[ElemType]])
                                            (implicit formatter: ElemTypeFormatter[ElemType]): Unit = {

    class LoggedMaxBufferLengthParserInput[ElemType](data: Iterator[IndexedSeq[ElemType]])
                                     (implicit formatter: ElemTypeFormatter[ElemType])
      extends IteratorParserInput[ElemType](data) {

      var maxInnerLength = 0

      override def dropBuffer(index: Int): Unit = {
        maxInnerLength = math.max(maxInnerLength, this.innerLength)
        super.dropBuffer(index)
      }
    }

    class LoggedDistributionBufferLengthParserInput[ElemType](data: Iterator[IndexedSeq[ElemType]])
                                                    (implicit formatter: ElemTypeFormatter[ElemType])
      extends IteratorParserInput[ElemType](data) {

      val drops = mutable.Map.empty[Int, Int].withDefaultValue(0)

      override def dropBuffer(index: Int): Unit = {
        drops(this.innerLength) = drops(this.innerLength) + 1
        super.dropBuffer(index)
      }
    }


    sizes.foreach(s => {
      val input = new LoggedMaxBufferLengthParserInput[ElemType](iteratorFactory(s))
      parser.parseInput(input)
      println(s"Batch size: $s. Max buffer size: ${input.maxInnerLength}.")
    })

    val input = new LoggedDistributionBufferLengthParserInput[ElemType](iteratorFactory(1))
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

  def benchmarkAll[ElemType](name: String,
                             parser: Parser[_, ElemType, _],
                             datas: Seq[IndexedSeq[ElemType]],
                             iteratorFactory: Int => Iterator[IndexedSeq[ElemType]])
                            (implicit formatter: ElemTypeFormatter[ElemType]): Unit = {

    /*val results = Utils.benchmark(s"$name Benchmark", datas.map(data => () => parser.parse(data)))
    println(results.map(_.mkString(" ")).mkString("\n"))*/

    val sizes = Seq(1, 2, /*4, 16, 64, */1024/*, 4096*/)
    Utils.benchmarkIteratorBufferSizes(parser, sizes, iteratorFactory)

    val iteratorResults = Utils.benchmark(s"$name Iterator Benchmark",
      sizes.map(s => () => parser.parseIterator(iteratorFactory(s)))
    )

    println(iteratorResults.map(_.mkString(" ")).mkString("\n"))

  }
}


