package benchmark

import java.util.concurrent.TimeUnit
import akka.actor.{ActorSystem, Props}
import datastructures.{ProbeTable, ProbeTableLongMap}
import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations.{OutputTimeUnit, _}
import org.openjdk.jmh.infra.Blackhole
import scala.util.Random

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@State(Scope.Thread)
class Repetition {

    @Param(Array("128"))
    var dimensions:Int = 0

    @Param(Array("16","24"))
    var k:Int = 0

    @Param(Array("100000"))
    var probetablesize:Int = 0

    var rnd:Random = new Random
    var vectors:Array[(Int, Array[Float])] = new Array(probetablesize)
    var hp:Hyperplane = new Hyperplane(k, rnd.nextLong(), dimensions)
    var probeTable = new ProbeTable(hp)
    var longMapProbeTable = new ProbeTableLongMap(hp)
    var rndVector = Array(0f)
    var repetition:Repetition = new Repetition
    var system:ActorSystem = _

    @Setup(Level.Trial)
    def setup(): Unit = {
      rnd = new Random(System.currentTimeMillis())
      system = ActorSystem("BenchmarkSystem")
      val a1 = system.actorOf(Props[actors.Repetition], name = "rep1")

      vectors = new Array(probetablesize)
      for (i <- vectors.indices) {
        vectors(i) = (rnd.nextInt, Array.fill[Float](dimensions)(rnd.nextFloat))
      }

      repetition = new Repetition
      hp = new Hyperplane(k, rnd.nextLong(), dimensions)

      for(v <- vectors) {

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
}
