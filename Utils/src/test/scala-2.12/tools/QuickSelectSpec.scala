package tools

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random
import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 30-04-2017.
  */
class QuickSelectSpec extends FlatSpec with Matchers {
  val fixure = {
    new {
      def getRndArrayList(size:Int, rnd:Random = new Random(System.currentTimeMillis())):DoubleArrayList = {
        val arr = new DoubleArrayList()
        var i = 0
        while(i < size) {
          arr.add(rnd.nextDouble)
          i+=1
        }
        arr
      }
    }
  }

  "quick select" should "return correct result " in {
    val v = new DoubleArrayList()
    v.add(3.0)
    v.add(3.1)
    v.add(4.2)
    v.add(0.001)
    assert(QuickSelect.selectKthDist(v, 0, v.size-1) == 0.001)
    assert(QuickSelect.selectKthDist(v, 1, v.size-1) == 3.0)
    assert(QuickSelect.selectKthDist(v, 2, v.size-1) == 3.1)
    assert(QuickSelect.selectKthDist(v, 3, v.size-1) == 4.2)
  }

  "quick select" should "never on random inputs crash" in {
    val f = fixure
    val rnd = new Random
    for(i <- 0 until 100000) {
      QuickSelect.selectKthDist(f.getRndArrayList(250), rnd.nextInt(250), 249)
      assert(1 == 1)
    }
  }

  "quick select" should "work on bad inputs (sorted)" in {
    val rnd = new Random
    val v = new DoubleArrayList
    val kSetSize = 250

    for(j <- 0 until kSetSize) {
      v.add(j.toDouble)
    }

    val r = QuickSelect.selectKthDist(v, 123, v.size-1)
    assert(r == 123.0)
    val z = QuickSelect.selectKthDist(v, 0, v.size-1)
    assert(z == 0.0)
  }

}
