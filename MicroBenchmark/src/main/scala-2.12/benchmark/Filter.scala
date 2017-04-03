package benchmark

import java.util

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

/**
  * Created by remeeh on 03-04-2017.
  */
@BenchmarkMode(Array(Mode.Throughput))
@State(Scope.Thread)
class Filter {
  var rnd:Random = new Random(System.currentTimeMillis())
  var vector:Array[Array[Float]] = Array()

  @Setup(Level.Iteration)
  def setup(): Unit = {
    vector = Array.fill[Array[Float]](200)(Array.fill[Float](128)(rnd.nextFloat))
  }

  @Benchmark
  def scalaFilter(bh:Blackhole) : Unit = {
    bh.consume(vector.filter(x => x.sum > 200f))
  }


  @Benchmark
  def custom(bh:Blackhole) : Unit = {
    var arrayBuffer = new Array[Array[Float]](vector.length)
    //bh.consume(c.apply(vector))
    var i = 0
    while(i < vector.length) {
      if(vector(i).sum > 200f) arrayBuffer(i) = vector(i)
      i+=1
    }
    bh.consume(arrayBuffer)
  }
}
