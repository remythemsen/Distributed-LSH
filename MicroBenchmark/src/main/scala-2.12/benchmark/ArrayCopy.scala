package benchmark

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import it.unimi.dsi.fastutil.ints.IntArrayList
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import tools.CandSet

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 16-05-2017.
  */

@State(Scope.Benchmark)
class ArrayCopy {
  var rnd:Random = _
  var a1i:Array[Int] = _
  var a1d:Array[Double] = _
  var alInt:IntArrayList = _
  var alDouble:DoubleArrayList = _
  val INITIAL_SIZE = 20000
  var candSet:CandSet = _

  @Setup(Level.Trial)
  def setup(): Unit = {
    this.rnd = new Random(System.currentTimeMillis())
    this.a1i = Array.fill[Int](INITIAL_SIZE)(rnd.nextInt())
    this.a1d = Array.fill[Double](INITIAL_SIZE)(rnd.nextDouble())
    this.candSet = new CandSet(INITIAL_SIZE * 10)
  }

  @Setup(Level.Invocation)
  def invocate():Unit = {
    this.candSet.reset
  }

  @Benchmark
  def addFill(bh:Blackhole) = {
    var j = 0
    while(j < this.a1i.length) {
      this.candSet+=(this.a1i(j),this.a1d(j))
      j+=1
    }
    bh.consume(this.candSet.ids)
  }

}
