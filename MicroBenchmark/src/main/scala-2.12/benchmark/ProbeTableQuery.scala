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

  @Param(Array("16","24"))
  var k:Int = 0

  @Param(Array("100000"))
  var probetablesize:Int = 0

  var rnd:Random = new Random
  var vectors:Array[(Int, Array[Float])] = new Array(probetablesize)
  var hp:Hyperplane = new Hyperplane(k, rnd.nextLong(), dimensions)
  var hp2:HyperplaneLong = new HyperplaneLong(k, rnd.nextLong(), dimensions)
  var probeTable = new ProbeTable(hp)
  var longMapProbeTable = new ProbeTableLong(hp2)
  var rndVector = Array(0f)

  @Setup(Level.Trial)
  def setup(): Unit = {
    rnd = new Random(System.currentTimeMillis())

    vectors = new Array(probetablesize)
    for (i <- vectors.indices) {
      vectors(i) = (rnd.nextInt, Array.fill[Float](dimensions)(rnd.nextFloat))
    }

    hp = new Hyperplane(k, rnd.nextLong(), dimensions)

    probeTable = new ProbeTableLong(hp2)

    longMapProbeTable = new ProbeTableLongMapOld(hp)

    for(v <- vectors) {
      probeTable += v
      longMapProbeTable += (v, 1)
    }

  }

  @Setup(Level.Invocation)
  def pickRndVectorKey(): Unit = {
    rndVector = vectors(rnd.nextInt(vectors.length))._2
  }

  // Remember to read from variable, and consume result by blackhole (avoid dead code eli)
  @Benchmark
  def query(bh:Blackhole):Unit = {
    bh.consume(probeTable.query(rndVector))
  }

  @Benchmark
  def queryLongMap(bh:Blackhole):Unit = {
    bh.consume(longMapProbeTable.query(rndVector))
  }
}
