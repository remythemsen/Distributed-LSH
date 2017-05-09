package benchmark

import java.util.concurrent.TimeUnit

import datastructures.Table
import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations.{OutputTimeUnit, _}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class ProbeTableQuery {
  @Param(Array("128"))
  var dimensions:Int = 0

  @Param(Array("16", "32"))
  var k:Int = 0

  @Param(Array("1000000", "20000000"))
  var probetablesize:Int = 0

  var rnd:Random = _
  var vectors:Array[(Int, Array[Float])] = _
  var hp:Hyperplane = _
  var hp2:Hyperplane = _
  var newTable:Table[Array[Float]] = _
  var rndVector:Array[Float] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    rnd = new Random(System.currentTimeMillis())
    hp = Hyperplane(k, rnd.nextLong(), dimensions)
    hp2 = Hyperplane(k, rnd.nextLong(), dimensions)
    newTable = new Table(hp2)
    vectors = new Array(probetablesize)

    var i = 0
    while(i < vectors.length) {
      vectors(i) = (rnd.nextInt, Array.fill[Float](dimensions)(rnd.nextFloat))
      newTable += (vectors(i))
      i += 1
    }
  }

  @Setup(Level.Invocation)
  def pickRndVectorKey(): Unit = {
    rndVector = vectors(rnd.nextInt(vectors.length))._2
  }

}
