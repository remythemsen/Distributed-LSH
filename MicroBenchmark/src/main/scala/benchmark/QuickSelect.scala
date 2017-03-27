package benchmark

import org.openjdk.jmh.annotations.BenchmarkMode
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

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
  var candSet:Array[(Int, Double)] = Array()
  var qsRnd = new Random

  @Setup(Level.Invocation)
  def genCandSet(): Unit = {
    candSet = Array.fill(n)(rnd.nextDouble).map(x => (rnd.nextInt(2000000), x))
    qsRnd = new Random(rnd.nextLong)
  }

  @Benchmark
  def tupleWithDouble(bh:Blackhole) : Unit = {
    bh.consume(tools.QuickSelect.quickSelect(candSet, n, qsRnd))
  }

}
