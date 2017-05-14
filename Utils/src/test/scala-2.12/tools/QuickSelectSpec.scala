package tools

import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 30-04-2017.
  */
class QuickSelectSpec extends FlatSpec with Matchers {

  "quick select" should "return correct result " in {
    val v = ArrayBuffer(3.0, 3.1, 4.2, 0.001)
    assert(QuickSelect.selectKthDist(v, 0, v.size) == 0.001)
    assert(QuickSelect.selectKthDist(v, 1, v.size) == 3.0)
    assert(QuickSelect.selectKthDist(v, 2, v.size) == 3.1)
    assert(QuickSelect.selectKthDist(v, 3, v.size) == 4.2)
  }

  "quick select" should "never on random inputs crash" in {
    val rnd = new Random
    for(i <- 0 until 100000) {
      QuickSelect.selectKthDist(ArrayBuffer.fill[Double](250)(rnd.nextDouble), rnd.nextInt(250), 250)
      assert(1 == 1)
    }
  }

  "quick select" should "work on bad inputs (sorted)" in {
    val rnd = new Random
    val v = new ArrayBuffer[(Int, Double)]
    val kSetSize = 250

    for(j <- 0 until kSetSize) {
      v+=Tuple2(rnd.nextInt, j.toDouble)
    }

    val r = QuickSelect.selectKthDist(v.map(_._2), 123, v.size)
    assert(r == 123.0)
  }

  "quick select" should "never crash on shuffled predefined inputs" in {
    val rnd = new Random
    val v = new ArrayBuffer[(Int, Double)]
    val kSetSize = 250
    for(j <- 0 until kSetSize) {
      v+=Tuple2(rnd.nextInt, j.toDouble)
    }
    for(i <- 0 until 100000) {
      val vs = rnd.shuffle(v)
      val r = QuickSelect.selectKthDist(vs.map(_._2), 123, vs.size)
      assert(r == 123.0)
    }
  }
}
