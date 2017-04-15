package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class HyperplaneSpec extends FlatSpec with Matchers {
  "Apply method" should "return a long" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }

  "Apply method" should "return correct result" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(1, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }


}
