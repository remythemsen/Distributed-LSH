package benchmark

import hashfunctions.Hyperplane
import multiprobing.{PQ, ProbeScheme}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class GenerateProbes {

  @Param(Array("8", "16", "32", "64"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var key:Array[Float] = Array()
  var hp:Hyperplane = Hyperplane(k, rnd.nextLong(), 128)
  var mp:PQ[Array[Float]] = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    hp = Hyperplane(k, rnd.nextLong(), 128)
    mp = new PQ(k, Array(hp))
    key = Array.fill(128)(rnd.nextFloat)
  }

  @Benchmark
  def pqProbes(bh:Blackhole):Unit = {
    bh.consume(mp.generate(key))
  }
}
