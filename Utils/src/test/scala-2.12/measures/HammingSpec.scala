package measures
import com.googlecode.javaewah.datastructure.BitSet
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class HammingSpec extends FlatSpec with Matchers {


  def fixture = {
    new {
      val rnd = new Random
      val dimensions = 256
    }
  }

  def randomBitSet(dim:Int, seed:Long):BitSet = {
    val rnd = new Random(seed)
    var i = 0
    var res = new BitSet(dim)
    while(i < dim) {
      if(rnd.nextBoolean()) {
        res.set(i)
      }
      i+=1
    }
    res
  }

  "hamming measure " should " return 0 on identical vectors!" in {
    val f = fixture
    val v1 = randomBitSet(f.dimensions, f.rnd.nextLong())
    assert(Hamming.measure(v1, v1) == 0.0)
  }

  "hamming measure " should " return correct precalculated results " in {
    val f = fixture
    val v1 = new BitSet(9)
    v1.set(1)
    v1.set(4)
    v1.set(6)
    v1.set(8)
    val v2 = new BitSet(9)
    v2.set(1)
    v2.set(4)
    v2.set(6)
    v2.set(8)
    val v3 = new BitSet(9)
    v3.set(1)
    v3.set(6)
    v3.set(8)
    val v4 = new BitSet(9)
    v4.set(1)
    v4.set(3)
    v4.set(6)
    v4.set(9)
    assert(Hamming.measure(v1, v2) == 0.0)
    assert(Hamming.measure(v1, v3) == 1.0)
    assert(Hamming.measure(v1, v4) == 4.0)
    assert(Hamming.measure(v3, v4) == 3.0)
  }

  "hamming measure " should " never be negative " in {
    val f = fixture
    for(i <- 0 until 100) {

      val v1 = randomBitSet(f.dimensions, f.rnd.nextLong())
      val v2 = randomBitSet(f.dimensions, f.rnd.nextLong())
      assert(Hamming.measure(v1, v2) >= 0.0)
    }
  }

}
