package benchmark

import java.util

import measures._
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class Distance {


  def getRndVec(dimensions:Int, seed:Long) = {
    val rnd = new Random(seed)
    val vec = new util.BitSet()
    for (i <- 0 until dimensions) {
      if (rnd.nextBoolean()) {
        vec.set(i)
      }
    }
    vec
  }

  @Param(Array("128", "256"))
  var dimensions:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var vector:Array[Float] = Array()
  var vector2:Array[Float] = Array()
  var bitVector:util.BitSet = _
  var bitVector2:util.BitSet = _
  var hamming:measures.Distance[util.BitSet] = _
  var euclidean:measures.Distance[Array[Float]] = _
  var euclideanFast:measures.Distance[Array[Float]] = _

  @Setup(Level.Trial)
  def onTrial():Unit = {
    hamming = Hamming
    euclidean = Euclidean
    euclideanFast = EuclideanFast
  }

  @Setup(Level.Invocation)
  def genRandomVec(): Unit = {
    vector = Array.fill[Float](dimensions)(rnd.nextFloat)
    vector2 = Array.fill[Float](dimensions)(rnd.nextFloat)
    bitVector = getRndVec(dimensions,rnd.nextLong)
    bitVector2 = getRndVec(dimensions,rnd.nextLong)
  }

  @Benchmark
  def euclidean(bh:Blackhole) : Unit = {
    bh.consume(Euclidean.measure(vector, vector2))
  }

  @Benchmark
  def euclideanFast(bh:Blackhole) : Unit = {
    bh.consume(EuclideanFast.measure(vector, vector2))
  }

  @Benchmark
  def hamming(bh:Blackhole) : Unit = {
    bh.consume(hamming.measure(bitVector, bitVector2))
  }
}
