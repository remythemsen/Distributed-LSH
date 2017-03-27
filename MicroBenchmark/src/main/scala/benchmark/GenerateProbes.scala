package benchmark

import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class GenerateProbes {

  @Param(Array("8", "16", "24"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var key:Array[Int] = Array()
  var hp:Hyperplane = new Hyperplane(k, rnd.nextLong(), 128)

  @Setup(Level.Iteration)
  def setup(): Unit = {
    hp = new Hyperplane(k, rnd.nextLong(), 128)
  }

  @Setup(Level.Invocation)
  def genKey(): Unit = {
    key = Array.fill(k)(rnd.nextInt(2))
  }

  @Benchmark
  def hyperplane(bh:Blackhole):Unit = {
    bh.consume(hp.generateProbes(key))
  }
}
