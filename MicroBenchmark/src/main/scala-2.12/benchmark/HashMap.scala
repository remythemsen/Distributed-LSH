package benchmark

import it.unimi.dsi.fastutil.ints.IntArrayList
import it.unimi.dsi.fastutil.longs.{Long2IntOpenHashMap, Long2ObjectOpenHashMap}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 16-05-2017.
  */
@State(Scope.Benchmark)
class HashMap {
  var rnd:Random = _
  var longMap:mutable.LongMap[Int] = _
  var longMapAB:mutable.LongMap[mutable.ArrayBuffer[Int]] = _
  var fastUtilMap:Long2IntOpenHashMap = _
  var fastUtilMapAL:Long2ObjectOpenHashMap[IntArrayList] = _
  var longMapKey:Long = _
  var longMapKeys:Array[Long] = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    this.rnd = new Random(System.currentTimeMillis())
    this.longMap = new mutable.LongMap()
    this.longMapAB = new mutable.LongMap()
    this.fastUtilMap = new Long2IntOpenHashMap()
    this.fastUtilMapAL = new Long2ObjectOpenHashMap()
    // Make set of keys
    this.longMapKeys = Array.fill[Long](20000000)(this.rnd.nextLong)
    // Populate the maps
    var i = 0
    while(i < longMapKeys.length) {
      val anInt = rnd.nextInt
      this.longMap.put(longMapKeys(i), anInt)
      this.longMapAB.put(longMapKeys(i), {
          ArrayBuffer(anInt)
        })
      this.fastUtilMap.put(longMapKeys(i), anInt)
      this.fastUtilMapAL.put(longMapKeys(i), {
        val al = new IntArrayList()
        al.add(anInt)
        al
      })
    }
  }

  @Setup(Level.Invocation)
  def getRandomVec(): Unit = {
    if (rnd.nextGaussian() < 0) {
      this.longMapKey = this.longMapKeys(this.rnd.nextInt(this.longMapKeys.length))
    } else {
      this.longMapKey = this.rnd.nextLong
    }
  }

  @Benchmark
  def longMapContains(bh:Blackhole) : Unit = {
    bh.consume(this.longMap.contains(this.longMapKey))
  }

  @Benchmark
  def longMapGet(bh:Blackhole) : Unit = {
    bh.consume(this.longMap.get(this.longMapKey))
  }

  @Benchmark
  def longMapABGet(bh:Blackhole) : Unit = {
    bh.consume(this.longMapAB.get(this.longMapKey))
  }

  @Benchmark
  def fastUtilMapContains(bh:Blackhole) : Unit = {
    bh.consume(this.fastUtilMap.containsKey(this.longMapKey))
  }

  @Benchmark
  def fastUtilMapGet(bh:Blackhole) : Unit = {
    bh.consume(this.fastUtilMap.get(this.longMapKey))
  }

  @Benchmark
  def fastUtilMapALGet(bh:Blackhole) : Unit = {
    bh.consume(this.fastUtilMapAL.get(this.longMapKey))
  }

}
