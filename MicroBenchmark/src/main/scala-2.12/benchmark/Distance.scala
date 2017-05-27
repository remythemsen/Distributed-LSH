package benchmark

import java.util

import com.googlecode.javaewah.datastructure.BitSet
import measures._
import org.apache.lucene.util.OpenBitSet
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.roaringbitmap.RoaringBitmap

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class Distance {

  def getRndVeco(dimensions:Int, seed:Long,size:Int) = {
    val rnd = new Random(seed)
    val arr = new Array[util.BitSet](size)
    for(j <- arr.indices) {
      val vec = new util.BitSet()
      for (i <- 0 until dimensions) {
        if (rnd.nextBoolean()) {
          vec.set(i)
        }
      }
      arr(j) = vec
    }
    arr
  }

  def getRndVec(dimensions:Int, seed:Long, size:Int) = {
    val rnd = new Random(seed)
    val arr = new Array[OpenBitSet](size)
    for (j <- arr.indices) {
      val vec = new OpenBitSet()
      for (i <- 0 until dimensions) {
        if (rnd.nextBoolean()) {
          vec.set(i)
        }
      }
      arr(j) = vec
    }
    arr
  }

  def getRndVece(dimensions:Int, seed:Long, size:Int) = {
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


  @Param(Array("128", "256", "512"))
  var dimensions:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var vector:Array[Float] = Array()
  var vector2:Array[Float] = Array()

  var eucVectorA:Array[Array[Float]] = _
  var bitVector:OpenBitSet = _
  var bitVector2:OpenBitSet = _
  var bitVectore:BitSet = _
  var bitVectore2:BitSet = _
  var bitVectoro:util.BitSet = _
  var bitVectoro2:util.BitSet = _
  var bitVectorr:RoaringBitmap = _
  var bitVectorr2:RoaringBitmap = _
  var bitVectorA:Array[OpenBitSet] = _
  var bitVectoreA:Array[BitSet] = _
  var bitVectoroA:Array[util.BitSet] = _
  var bitVectorrA:Array[RoaringBitmap] = _

  var hamming:measures.Distance[BitSet] = _
  var hamming2:measures.Distance[OpenBitSet] = _
  var hamming3:measures.Distance[util.BitSet] = _
  var hamming4:measures.Distance[RoaringBitmap] = _
  var euclidean:measures.Distance[Array[Float]] = _
  var euclideanFast:measures.Distance[Array[Float]] = _

  @Setup(Level.Trial)
  def onTrial():Unit = {
    hamming = Hamming
    hamming2 = Hamming2
    hamming3 = Hamming3
    hamming4 = Hamming4
    euclidean = Euclidean
    euclideanFast = EuclideanFast
    // Generating vectors
    eucVectorA = Array.fill[Array[Float]](100000)(Array.fill[Float](dimensions)(rnd.nextFloat))
    bitVectorA = getRndVec(dimensions, rnd.nextLong(), 100000)
    bitVectoreA = getRndVece(dimensions, rnd.nextLong(), 100000)
    bitVectoroA = getRndVeco(dimensions, rnd.nextLong(), 100000)


  }

  @Setup(Level.Iteration)
  def genRandomVec(): Unit = {
    vector = eucVectorA(rnd.nextInt(eucVectorA.length))
    vector2 = eucVectorA(rnd.nextInt(eucVectorA.length))
    bitVector = bitVectorA(rnd.nextInt(bitVectorA.length))
    bitVector2 = bitVectorA(rnd.nextInt(bitVectorA.length))
    bitVectore = bitVectoreA(rnd.nextInt(bitVectoreA.length))
    bitVectore2 = bitVectoreA(rnd.nextInt(bitVectoreA.length))
    bitVectoro = bitVectoroA(rnd.nextInt(bitVectoreA.length))
    bitVectoro2 = bitVectoroA(rnd.nextInt(bitVectoreA.length))
  }

  @Benchmark
  def euclidean(bh:Blackhole) : Unit = {
    bh.consume(Euclidean.measure(vector, vector2))
  }


  @Benchmark
  def LuceneBitSet(bh:Blackhole) : Unit = {
    bh.consume(hamming2.measure(bitVector, bitVector2))
  }

  @Benchmark
  def euclideanFast(bh:Blackhole) : Unit = {
    bh.consume(EuclideanFast.measure(vector, vector2))
  }
  @Benchmark
  def utilBitSet(bh:Blackhole) : Unit = {
    bh.consume(hamming3.measure(bitVectoro, bitVectoro2))
  }

  @Benchmark
  def javaEWAHBitSet(bh:Blackhole) : Unit = {
    bh.consume(hamming.measure(bitVectore, bitVectore2))
  }
}
