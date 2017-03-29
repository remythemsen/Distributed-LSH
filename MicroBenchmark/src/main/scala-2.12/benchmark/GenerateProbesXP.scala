package benchmark

import hashfunctions.Crosspolytope
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class GenerateProbesXP {

  @Param(Array("2", "3", "4"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var key:Array[Int] = Array()
  var xp:Crosspolytope = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    xp = new Crosspolytope(k, rnd.nextLong(), 128)
  }

  @Setup(Level.Invocation)
  def genKey(): Unit = {
    key = xp(Array.fill[Float](128)(rnd.nextFloat()))
  }

  @Benchmark
  def hyperplane(bh:Blackhole):Unit = {
    bh.consume(xp.generateProbes(key))
  }
}
