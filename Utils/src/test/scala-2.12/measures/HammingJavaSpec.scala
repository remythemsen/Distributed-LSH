package measures

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random
import java.util.BitSet

/**
  * Created by remeeh on 16-04-2017.
  */
class HammingJavaSpec extends FlatSpec with Matchers {


  def fixture = {
    new {
      val rnd = new Random
      val dimensions = 256
    }
  }

  def randomBitSet(dim:Int, seed:Long):BitSet = {
    val rnd = new Random(seed)
    var i = 0
    var res = new BitSet
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
    assert(new HammingJava().measure(v1, v1) == 0.0)
  }

  "hamming measure " should " return correct precalculated results " in {
    val f = fixture
    val v1 = new BitSet
    v1.set(1)
    v1.set(2)
    v1.set(3)
    val v2 = new BitSet
    v2.set(0)
    v2.set(1)
    v2.set(2)
    val v3 = new BitSet
    v3.set(1)
    v3.set(2)
    v3.set(3)
    val v4 = new BitSet
    v4.set(4)
    v4.set(5)
    v4.set(6)

    assert(new HammingJava().measure(v1, v2) == 2.0)
    assert(new HammingJava().measure(v1, v3) == 0.0)
    assert(new HammingJava().measure(v3, v4) == 6.0)
  }

  "hamming measure " should " never be negative " in {
    val f = fixture
    for(i <- 0 until 100) {

      val v1 = randomBitSet(f.dimensions, f.rnd.nextLong())
      val v2 = randomBitSet(f.dimensions, f.rnd.nextLong())
      assert(new HammingJava().measure(v1, v2) >= 0.0)
    }
  }

}
