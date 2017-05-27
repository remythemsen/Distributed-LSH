package benchmark

import com.googlecode.javaewah.datastructure.BitSet
import hashfunctions.{BitHash, Hyperplane}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class HashFunction {

  @Param(Array("128", "256", "512"))
  var dimensions:Int = 0

  @Param(Array("8", "16", "24", "32"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var hp:Hyperplane = _
  var bhs:BitHash = _
  var vector:Array[Float] = _
  var bitVector:BitSet = _
  var vectors:Array[Array[Float]] = _
  var bitVectors:Array[BitSet] = _

  def genRndVece(dimensions:Int, seed:Long, size:Int) = {
    val rnd = new Random(seed)
    val arr = new Array[BitSet](size)
    for (j <- arr.indices) {
      val vec = new BitSet(dimensions)
      for (i <- 0 until dimensions) {
        if (rnd.nextBoolean()) {
          vec.set(i)
        }
      }
      arr(j) = vec
    }
    arr
  }

  @Setup(Level.Trial)
  def setup(): Unit = {
    vectors = Array.fill[Array[Float]](50000)(Array.fill[Float](dimensions)(rnd.nextFloat()))
    bitVectors = genRndVece(dimensions, rnd.nextLong, 50000)
    hp = Hyperplane(k, rnd.nextLong(), dimensions)
    bhs = BitHash(k, rnd.nextLong(), dimensions)
  }

  @Setup(Level.Iteration)
  def genRandomVec(): Unit = {
    vector = vectors(rnd.nextInt(vectors.length))
    bitVector = bitVectors(rnd.nextInt(vectors.length))
  }

  @Benchmark
  def hyperplane(bh:Blackhole) : Unit = {
    bh.consume(hp.apply(vector))
  }

  @Benchmark
  def bithash(bh:Blackhole) : Unit = {
    bh.consume(bhs.apply(bitVector))
  }

}
