package tools

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 30-04-2017.
  */
class QuickSelectSpec extends FlatSpec with Matchers {
  "quick select" should "return results even on set with duplicates " in {
    val v = ArrayBuffer((0,1.0,0),(0,1.0,0),(0,1.0,0),(0,1.0,0))
    //val res:Double = SAQuickSelect.quickSelect(v,2)
    val res:Double = QuickSelect.selectKthDist(v, 2)
    assert(res == 1.0)
  }

  "quick select" should "return correct result " in {
    val v = ArrayBuffer((131, 3.0, 2),(432, 3.1, 3),(421, 4.2, 4),(543, 0.001, 9))
    assert(QuickSelect.selectKthDist(v, 0) == 0.001)
    assert(QuickSelect.selectKthDist(v, 1) == 3.0)
    assert(QuickSelect.selectKthDist(v, 2) == 3.1)
    assert(QuickSelect.selectKthDist(v, 3) == 4.2)
  }
}
