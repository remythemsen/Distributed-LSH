package benchmark

import it.unimi.dsi.fastutil.ints.IntOpenHashSet
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable
import scala.util.Random

@State(Scope.Benchmark)
class HashSet {
  var rnd:Random = _
  var jhs:mutable.HashSet[Int] = _
  var fuhs:IntOpenHashSet = _
  var key:Int = _
  var keys:Array[Int] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    this.rnd = new Random(System.currentTimeMillis())
    this.jhs = new mutable.HashSet()
    this.fuhs = new IntOpenHashSet()
    // Make set of keys
    this.keys = Array.fill[Int](10000000)(this.rnd.nextInt)
    var i = 0
    while(i < this.keys.length) {
      this.jhs.add(this.keys(i))
      this.fuhs.add(this.keys(i))
      i+=1
    }
  }

  @Setup(Level.Iteration)
  def getRandom(): Unit = {
    if (rnd.nextGaussian() < 0) {
      this.key = this.keys(this.rnd.nextInt(this.keys.length))
    } else {
      this.key = this.rnd.nextInt
    }
  }

  @Benchmark
  def scalaHashSetContains(bh:Blackhole) : Unit = {
    bh.consume(this.jhs.contains(this.key))
  }

  @Benchmark
  def scalaHashSetAdd(bh:Blackhole) : Unit = {
    bh.consume(this.jhs.add(this.rnd.nextInt()))
  }

  @Benchmark
  def fastUtilHashSetContains(bh:Blackhole) : Unit = {
    bh.consume(this.fuhs.contains(this.key))
  }

  @Benchmark
  def fastUtilHashSetAdd(bh:Blackhole) : Unit = {
    bh.consume(this.fuhs.add(this.rnd.nextInt()))
  }

}
