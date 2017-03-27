package benchmark

import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
class Hash {
  @State(Scope.Thread)
  class HyperplaneHash {
    @Param(Array("128", "256"))
    var dimensions:Int = 0

    @Param(Array("8", "16", "24"))
    var k:Int = 0

    var rnd:Random = new Random(System.currentTimeMillis())
    var hp:Hyperplane = new Hyperplane(1, rnd.nextLong(), dimensions)
    var vector:Array[Float] = Array()

    @Setup(Level.Iteration)
    def setup(): Unit = {
      hp = new Hyperplane(k, rnd.nextLong(), dimensions)
    }

    @Setup(Level.Invocation)
    def genRandomVec(): Unit = {
      vector = Array.fill[Float](dimensions)(rnd.nextFloat)
    }

    @Benchmark
    def run(bh:Blackhole) : Unit = {
      bh.consume(hp.apply(vector))
    }

  }
//  @State(Scope.Thread)
//  class CrossPolyHash {
//
//  }
}
