package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class BitHashSpec extends FlatSpec with Matchers {
  "Apply method" should "return a long" in {
    val rnd = new Random(System.currentTimeMillis())
    val k = 6
    val hp = BitHash(k, rnd.nextLong(), 128)
    val vec = mutable.BitSet()
    for(i <- 0 until k) {
      // TODO is this correct order
      vec(i) = rnd.nextBoolean()
    }
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }

  // TODO Make test for bithash correctness

}
