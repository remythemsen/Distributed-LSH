package tools

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

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
    val v = new CandSet(4)
    v.+=(4,3.0)
    v.+=(3,3.1)
    v.+=(2,4.2)
    v.+=(1,0.001)
    assert(new QuickSelect().selectKthDist(v, 0, v.size-1) == 0.001)
    assert(new QuickSelect().selectKthDist(v, 1, v.size-1) == 3.0)
    assert(new QuickSelect().selectKthDist(v, 2, v.size-1) == 3.1)
    assert(new QuickSelect().selectKthDist(v, 3, v.size-1) == 4.2)
  }

  "quick select" should "never on random inputs crash" in {
    val f = fixure
    val rnd = new Random
    val cands = new CandSet(10000)
    for(i <- 0 until 100000) {
      for(j <- 0 until 250) {
        cands+=(rnd.nextInt(),rnd.nextDouble())
      }
      new QuickSelect().selectKthDist(cands, rnd.nextInt(250), 249)
      assert(1 == 1)
    }
  }

  "quick select" should "work on bad inputs (sorted)" in {
    val rnd = new Random
    val kSetSize = 250
    val cands = new CandSet(10000)

    for(j <- 0 until kSetSize) {
      cands.+=(rnd.nextInt(), j.toDouble)
    }

    val r = new QuickSelect().selectKthDist(cands, 123, cands.size-1)
    assert(r == 123.0)
    val z = new QuickSelect().selectKthDist(cands, 0, cands.size-1)
    assert(z == 0.0)
  }

  "quick select" should "never go swap outside of until" in {
    val rnd = new Random
    val qs = new QuickSelect()
    val cs = new CandSet(20)
    for(i <- 1 until 21) {
      cs+=(20-i, 20-i.toDouble)
    }
    cs+=(122, -0.001)
    cs+=(121, -0.0091)
    cs.take(16)
    val r = qs.selectKthDist(cs, 16-1, cs.size-1)
    assert(1 == 1)
  }

}
