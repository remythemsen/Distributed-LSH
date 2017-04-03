package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class CrosspolytopeSpec extends FlatSpec with Matchers {
  "Apply method" should "return an array of int" in {
    val rnd = new Random(System.currentTimeMillis())
    val xp = new Crosspolytope(3, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    assert(xp(vec).getClass == new Array[Int](6).getClass)
  }

  "Apply method" should "return array of length equal to k" in {
    val k = 3
    val rnd = new Random(System.currentTimeMillis())
    val xp = new Crosspolytope(k, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    assert(xp(vec).length == k)
  }

  " " should "Return" in {
    val k = 3
    val rnd = new Random(System.currentTimeMillis())
    val xp = new Crosspolytope(k, rnd.nextLong(), 128)
    //xp.generateSets()
  }

  "generateProbes method" should "generate an array of an array of ints" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Crosspolytope(3, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val pbs = hp.generateProbes(hp(vec))
    assert(pbs.getClass == new Array[Array[Int]](6).getClass)
  }


}
