package benchmark

import hashfunctions.{Hyperplane, HyperplaneLong}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class GenerateProbes {

  @Param(Array("8", "16", "32", "64"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var key:Array[Int] = Array()
  var key2:Long = _
  var hp:Hyperplane = new Hyperplane(k, rnd.nextLong(), 128)
  var hp2:HyperplaneLong = new HyperplaneLong(k, rnd.nextLong(), 128)

  @Setup(Level.Iteration)
  def setup(): Unit = {
    hp = new Hyperplane(k, rnd.nextLong(), 128)
    hp2 = new HyperplaneLong(k, rnd.nextLong(), 128)
  }

  @Setup(Level.Invocation)
  def genKey(): Unit = {
    key = hp(Array.fill(128)(rnd.nextFloat))
    key2 = hp2(Array.fill(128)(rnd.nextFloat))
  }

  @Benchmark
  def hyperplane(bh:Blackhole):Unit = {
    bh.consume(hp.generateProbes(key))
  }
  @Benchmark
  def hyperplaneLong(bh:Blackhole):Unit = {
    bh.consume(hp2.generateProbes(key2))
  }
}
