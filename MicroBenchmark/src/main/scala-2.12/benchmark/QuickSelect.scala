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
  var arrCandSet:Array[(Int, Double)] = Array()
  var arrCandSetSingle:Array[Double] = Array()
  var qsRnd = new Random
  var k = 0

  @Setup(Level.Invocation)
  def genCandSet(): Unit = {
    candSet = ArrayBuffer.fill(n)(rnd.nextDouble).map(x => (rnd.nextInt(2000000), x))
    arrCandSet = candSet.toArray
    arrCandSetSingle = candSet.toArray.map(x => x._2)
    qsRnd = new Random(rnd.nextLong)
    k = qsRnd.nextInt(n)
  }

  @Benchmark
  def genericQSArrayBufferTuple(bh:Blackhole) : Unit = {
    bh.consume(tools.QuickSelect.quickSelect(candSet, k, qsRnd))
  }

  @Benchmark
  def specializedQSArrayBufferTuple(bh:Blackhole) : Unit = {
    bh.consume(tools.SQuickSelect.quickSelect(arrCandSet, k, qsRnd))
  }

  @Benchmark
  def specializedQSArraySingle(bh:Blackhole) : Unit = {
    bh.consume(tools.SSQuickSelect.quickSelect(arrCandSetSingle, k, qsRnd))
  }

}
