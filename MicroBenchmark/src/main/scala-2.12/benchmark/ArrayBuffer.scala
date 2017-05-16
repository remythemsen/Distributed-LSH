package benchmark

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import scala.util.Random

/**
  * Created by remeeh on 16-05-2017.
  */
class ArrayBuffer {
  @Param(Array("128", "256"))
  var dimensions:Int = 0

  var rnd:Random = new Random(System.currentTimeMillis())

  @Setup(Level.Iteration)
  def setup(): Unit = {
  }

  @Setup(Level.Invocation)
  def getRandomVec(): Unit = {
  }

  @Benchmark
  def longMap(bh:Blackhole) : Unit = {
  }
  @Benchmark
  def fastUtil(bh:Blackhole) : Unit = {
  }

}
