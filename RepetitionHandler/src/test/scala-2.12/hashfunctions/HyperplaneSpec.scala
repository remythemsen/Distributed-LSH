package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class HyperplaneSpec extends FlatSpec with Matchers {
  "Apply method" should "return an array of int" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    assert(hp(vec).getClass == new Array[Int](6).getClass)
  }

  "generateProbes method" should "generate an array of ints" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(10, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val pbs = hp.generateProbes(hp(vec))
    assert(pbs.getClass == new Array[Array[Int]](6).getClass)
  }

  "generateProbes method" should "make set of 1+(k*(k+1)/2)" in {
    val k = 10
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(k, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val pbs = hp.generateProbes(hp(vec))
    assert(pbs.length == (k*(k+1)/2)+1)
  }


}
