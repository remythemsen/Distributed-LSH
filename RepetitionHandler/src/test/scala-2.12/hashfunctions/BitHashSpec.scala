package hashfunctions

import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable
import scala.util.Random

class BitHashSpec extends FlatSpec with Matchers {
  def fixture = {
    new {
      val dimensions = 128
      val rnd = new Random(System.currentTimeMillis())

      def getRndVec(dimensions:Int, seed:Long) = {
        val rnd = new Random(seed)
        val vec = new mutable.BitSet()
        for(i <- 0 until dimensions) {
          if(rnd.nextBoolean()) {
            vec(i) = true
          }
        }
        vec
      }
    }
  }

  "Apply method" should "return a valid key" in {
    val f = fixture
    val validKeys = Array(0l, 1l, 2l, 3l)
    val hf = BitHash(2, f.rnd.nextLong(), f.dimensions)
    for(i <- 0 until 15) {
      val r = hf(f.getRndVec(f.dimensions, f.rnd.nextLong()))
      assert(validKeys.contains(r))
    }
  }

  "Apply method" should "return a long" in {
    val f = fixture
    val hf = BitHash(6, f.rnd.nextLong(), f.dimensions)
    val l:Long = 0
    val vec = f.getRndVec(f.dimensions, f.rnd.nextLong())
    val r = hf(vec)
    assert(r.getClass == l.getClass)
  }

  "Apply method" should "always return the same key from a vector" in {
    val f = fixture
    val hf = BitHash(6, f.rnd.nextLong(), f.dimensions)
    val vec = f.getRndVec(f.dimensions, f.rnd.nextLong())
    val firstKey = hf(vec)
    for(i <- 0 until 15) {
      assert(firstKey.equals(hf(vec)))
    }
  }

}
