package benchmark

/**
  * Created by remeeh on 24-03-2017.
  */
import java.util.concurrent.TimeUnit

import datastructures.ProbeTable
import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations.{OutputTimeUnit, _}
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.AverageTime))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Benchmark)
class HyperplaneHash {

  @Param(Array("128", "256"))
  var dimensions:Int = 0

  @Param(Array("8", "16", "24"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var vector:Array[Float] = Array()
  var hp:Hyperplane = new Hyperplane(1, rnd.nextLong(), dimensions)

  @Setup
  def setup(): Unit = {
    vector = Array.fill[Float](dimensions)(rnd.nextFloat)
    hp = new Hyperplane(k, rnd.nextLong(), dimensions)
  }

  // Remember to read from variable, and consume result by blackhole (avoid dead code eli)
  @Benchmark
  def generateProbes(bh:Blackhole):Unit = {
    bh.consume(hp.generateProbes(vector))
  }

  @Benchmark
  def hash(bh:Blackhole):Unit = {
    bh.consume(hp.apply(vector))
  }

}
