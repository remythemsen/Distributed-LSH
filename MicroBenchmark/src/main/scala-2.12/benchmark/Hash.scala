package benchmark

import hashfunctions.{Crosspolytope, Hyperplane}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class Hash {
  @Param(Array("128", "256"))
  var dimensions:Int = 0

  @Param(Array("8", "16", "24", "32"))
  var k:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())
  var hp:Hyperplane = _
  var hp2:Hyperplane = _
  //var c:Crosspolytope = _
  var vector:Array[Float] = _

  @Setup(Level.Iteration)
  def setup(): Unit = {
    hp = new Hyperplane(k, rnd.nextLong(), dimensions)
    hp2 = new Hyperplane(k, rnd.nextLong(), dimensions)
    //c = new Crosspolytope(k/4, rnd.nextLong(), dimensions)
  }

  @Setup(Level.Invocation)
  def genRandomVec(): Unit = {
    vector = Array.fill[Float](dimensions)(rnd.nextFloat)
  }

  @Benchmark
  def hyperplane(bh:Blackhole) : Unit = {
    bh.consume(hp.apply(vector))
  }

  @Benchmark
  def hyperplaneLong(bh:Blackhole) : Unit = {
    bh.consume(hp2.apply(vector))
  }

/*  @Benchmark
  def crosspolytope(bh:Blackhole) : Unit = {
    bh.consume(c.apply(vector))
  }*/
}
