package benchmark

import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 27-03-2017.
  */
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class QuickSelect {

  @Param(Array("100", "2000", "10000", "50000"))
  var n:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var candSet:ArrayBuffer[(Int, Double)] = ArrayBuffer()
  var arrCandSet:Array[Double] = Array()
  var qsRnd = new Random

  @Setup(Level.Invocation)
  def genCandSet(): Unit = {
    candSet = ArrayBuffer.fill(n)(rnd.nextDouble).map(x => (rnd.nextInt(2000000), x))
    arrCandSet = candSet.toArray.map(x => x._2)
    qsRnd = new Random(rnd.nextLong)
  }

  @Benchmark
  def genericQSArrayBufferTuple(bh:Blackhole) : Unit = {
    bh.consume(tools.QuickSelect.quickSelect(candSet, n, qsRnd))
  }

  @Benchmark
  def specializedQSArrayBufferTuple(bh:Blackhole) : Unit = {
    bh.consume(tools.SQuickSelect.quickSelect(candSet, n, qsRnd))
  }

  @Benchmark
  def specializedQSArraySingle(bh:Blackhole) : Unit = {
    bh.consume(tools.SSQuickSelect.quickSelect(arrCandSet, n, qsRnd))
  }

}
