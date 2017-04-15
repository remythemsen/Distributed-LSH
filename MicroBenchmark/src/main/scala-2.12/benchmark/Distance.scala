package benchmark

import measures.{Cosine, CosineUnit, Euclidean}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class Distance {
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
  def euclidean(bh:Blackhole) : Unit = {
    bh.consume(Euclidean.measure(vector, vector2))
  }

  @Benchmark
  def cosine(bh:Blackhole) : Unit = {
    bh.consume(Cosine.measure(vector, vector2))
  }

  @Benchmark
  def cosineUnit(bh:Blackhole) : Unit = {
    bh.consume(CosineUnit.measure(vector, vector2))
  }
}
