package benchmark

import hashfunctions.Hyperplane
import measures.Distance
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class DotProduct {
  @Param(Array("128", "256"))
  var dimensions:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var vector:Array[Float] = Array()
  var vector2:Array[Float] = Array()

  @Setup(Level.Invocation)
  def genRandomVec(): Unit = {
    vector = Array.fill[Float](dimensions)(rnd.nextFloat)
    vector2 = Array.fill[Float](dimensions)(rnd.nextFloat)
  }

  @Benchmark
  def parDot(bh:Blackhole) : Unit = {
    bh.consume(Distance.parDotProduct(vector, vector2))
  }

  @Benchmark
  def SpecDot(bh:Blackhole) : Unit = {
    bh.consume(Distance.dotProduct(vector, vector2))
  }
}
