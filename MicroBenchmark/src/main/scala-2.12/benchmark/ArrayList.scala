package benchmark

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
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
class ArrayList {
  var rnd:Random = _
  var abInt:ArrayBuffer[Int] = _
  var abDouble:ArrayBuffer[Double] = _
  var alInt:IntArrayList = _
  var alDouble:DoubleArrayList = _
  var nxtIndex:Int = _
  var nxtInt:Int = _
  var nxtDouble:Double = _
  val INITIAL_SIZE = 2000000

  @Setup(Level.Trial)
  def setup(): Unit = {
    this.rnd = new Random(System.currentTimeMillis())
    this.abInt = ArrayBuffer.fill[Int](INITIAL_SIZE)(this.rnd.nextInt)
    this.abDouble = ArrayBuffer.fill[Double](INITIAL_SIZE)(this.rnd.nextDouble)
    this.alInt = {
      val arr = new IntArrayList()
      for(i <- 0 until INITIAL_SIZE) {
        arr.add(this.rnd.nextInt())
      }
      arr
    }
    this.alDouble = {
      val arr = new DoubleArrayList()
      for(i <- 0 until INITIAL_SIZE) {
        arr.add(this.rnd.nextDouble())
      }
      arr
    }
  }

  @Setup(Level.Invocation)
  def invocate():Unit = {
    this.nxtIndex = this.rnd.nextInt(INITIAL_SIZE)
    this.nxtInt = this.rnd.nextInt(INITIAL_SIZE)
    this.nxtDouble = this.rnd.nextDouble
  }

  @Benchmark
  def abIntUpdate(bh:Blackhole) = {
    this.abInt.update(this.nxtIndex, this.nxtInt)
    bh.consume(this.abInt(nxtIndex))
  }

  @Benchmark
  def abDoubleUpdate(bh:Blackhole) = {
    this.abDouble.update(this.nxtIndex, this.nxtDouble)
    bh.consume(this.abDouble(nxtIndex))
  }

  @Benchmark
  def fuIntUpdate(bh:Blackhole) = {
    this.alInt.set(this.nxtIndex, this.nxtInt)
    bh.consume(this.alInt.getInt(nxtIndex))
  }

  @Benchmark
  def fuDoubleUpdate(bh:Blackhole) = {
    this.alDouble.set(this.nxtIndex, this.nxtDouble)
    bh.consume(this.alDouble.getDouble(nxtIndex))
  }

}
