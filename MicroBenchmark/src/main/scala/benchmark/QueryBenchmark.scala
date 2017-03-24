package benchmark

/**
  * Created by remeeh on 24-03-2017.
  */
import hashfunctions.Hyperplane
import org.openjdk.jmh.annotations._


import scala.util.Random
class QueryBenchmark {

  @Benchmark
  def generateProbes:Unit = {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(10, rnd.nextLong(), 128)
    val pbs = hp.generateProbes(getVector)
  }

  def getVector:Array[Float] = {
    val rnd = new Random(System.currentTimeMillis())
    Array.fill[Float](128)(rnd.nextFloat)
  }
}
