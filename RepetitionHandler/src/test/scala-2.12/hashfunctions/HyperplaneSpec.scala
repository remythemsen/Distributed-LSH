package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class HyperplaneSpec extends FlatSpec with Matchers {
  "Apply method" should "return a long" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }

  "Apply method" should "always return the same key from a vector" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val firstKey = hp(vec)
    for(i <- 0 until 15) {
      assert(firstKey.equals(hp(vec)))
    }
  }

  "Apply method" should "return a valid key" in {
    val rnd = new Random(System.currentTimeMillis())
    val validKeys = Array(0l, 1l, 2l, 3l)
    val hp = Hyperplane(2, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    for(i <- 0 until 15) {
      assert(validKeys.contains(hp(vec)))
    }
  }

}
