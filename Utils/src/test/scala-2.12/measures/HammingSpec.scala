package measures

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random

/**
  * Created by remeeh on 16-04-2017.
  */
class HammingSpec extends FlatSpec with Matchers {


  def fixture = {
    new {
      val rnd = new Random
      val dimensions = 256
    }
  }

  def randomBitSet(dim:Int, seed:Long):mutable.BitSet = {
    val rnd = new Random(seed)
    var i = 0
    var res = new mutable.BitSet
    while(i < dim) {
      if(rnd.nextBoolean()) {
        res.add(i)
      }
      i+=1
    }
    res
  }

  "hamming measure " should " return 0 on identical vectors!" in {
    val f = fixture
    val v1 = randomBitSet(f.dimensions, f.rnd.nextLong())
    assert(new Hamming(f.dimensions).measure(v1, v1) == 0.0)
  }

  "hamming measure " should " return correct precalculated results " in {
    val f = fixture
    val v1 = mutable.BitSet(1,4,6,8)
    val v2 = mutable.BitSet(1,4,6,8)
    val v3 = mutable.BitSet(1,6,8)
    val v4 = mutable.BitSet(1,3,6,9)
    assert(new Hamming(f.dimensions).measure(v1, v2) == 0.0)
    assert(new Hamming(f.dimensions).measure(v1, v3) == 1.0)
    assert(new Hamming(f.dimensions).measure(v1, v4) == 4.0)
    assert(new Hamming(f.dimensions).measure(v3, v4) == 3.0)
  }

  "hamming measure " should " never be negative " in {
    val f = fixture
    for(i <- 0 until 100) {

      val v1 = randomBitSet(f.dimensions, f.rnd.nextLong())
      val v2 = randomBitSet(f.dimensions, f.rnd.nextLong())
      assert(new Hamming(f.dimensions).measure(v1, v2) >= 0.0)
    }
  }

}
