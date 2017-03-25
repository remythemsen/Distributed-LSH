package benchmark

/**
  * Created by remeeh on 24-03-2017.
  */
import java.util.concurrent.TimeUnit

import datastructures.ProbeTable
import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import org.openjdk.jmh.runner.Runner
import org.openjdk.jmh.runner.RunnerException
import org.openjdk.jmh.runner.options.Options
import org.openjdk.jmh.runner.options.OptionsBuilder
import org.openjdk.jmh.annotations.OutputTimeUnit

import scala.util.Random
@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class HyperplaneQuery {

  @Param(Array("128"))
  var dimensions:Int = 0

  @Param(Array("128"))
  var k:Int = 0

  @Param(Array("10000", "100000"))
  var probetablesize:Int = 0

  var rnd:Random = new Random
  var vector:Array[Float] = Array()
  var vectors:Array[(Int, Array[Float])] = new Array(probetablesize)
  var hp:Hyperplane = new Hyperplane(k, rnd.nextLong(), dimensions)
  var probeTable = new ProbeTable(hp)
  var rndId = 0

  @Setup
  def setup(): Unit = {
    rnd = new Random(System.currentTimeMillis())

    vectors = new Array(probetablesize)
    for (i <- vectors.indices) {
      vectors(i) = (rnd.nextInt, Array.fill[Float](dimensions)(rnd.nextFloat))
    }

    hp = new Hyperplane(k, rnd.nextLong(), dimensions)
    probeTable = new ProbeTable(hp)

    for(v <- vectors) {
      probeTable += v
    }

    rndId = rnd.nextInt(vectors.length)
  }

  // Remember to read from variable, and consume result by blackhole (avoid dead code eli)
  @Benchmark
  def query(bh:Blackhole, rndId:Int):Unit = {
    bh.consume(probeTable.query(vectors(rndId)._2))
  }
}
