package benchmark

import java.util.concurrent.TimeUnit

import datastructures.{ProbeTable, ProbeTableLong, ProbeTableLongMapOld}
import hashfunctions.{Hyperplane, HyperplaneLong}
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
  var hp2:HyperplaneLong = _
  var oldTable:ProbeTableLongMapOld = _
  var newTable:ProbeTableLong = _
  var rndVector:Array[Float] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    rnd = new Random(System.currentTimeMillis())
    hp = new Hyperplane(k, rnd.nextLong(), dimensions)
    hp2 = new HyperplaneLong(k, rnd.nextLong(), dimensions)
    oldTable = new ProbeTableLongMapOld(hp, 30*k)
    newTable = new ProbeTableLong(hp2, 30*k)
    vectors = new Array(probetablesize)

    var i = 0
    while(i < vectors.length) {
      vectors(i) = (rnd.nextInt, Array.fill[Float](dimensions)(rnd.nextFloat))
      oldTable += (vectors(i), 1)
      newTable += (vectors(i), 1)
      i += 1
    }
  }

  @Setup(Level.Invocation)
  def pickRndVectorKey(): Unit = {
    rndVector = vectors(rnd.nextInt(vectors.length))._2
  }

  // Remember to read from variable, and consume result by blackhole (avoid dead code eli)
  @Benchmark
  def queryLongMapOld(bh:Blackhole):Unit = {
    bh.consume(oldTable.query(rndVector))
  }

  @Benchmark
  def queryLongMapNew(bh:Blackhole):Unit = {
    bh.consume(newTable.query(rndVector))
  }
}
