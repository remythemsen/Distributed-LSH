package tools

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by remeeh on 17-05-2017.
  */
class CandSetSpec extends FlatSpec with Matchers {
  "candSet " should " hold a single candidate when one is inserted" in {
    val c = new CandSet(10)
    c+=(5, 1.0)
    assert(c.ids.getInt(0) == 5)
  }

  "candSet " should " hold 0 candidates when none is inserted" in {
    val c = new CandSet(10)
    assert(c.size == 0)
  }

  "candSet " should " return the correct size" in {
    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(2,1.0)
    c+=(3,1.0)
    c+=(4,1.0)
    c+=(5,1.0)
    assert(c.size == 5)
  }

  "candSet " should " only have unique candidates when using +=" in {
    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(1,2.0)
    c+=(1,3.0)
    c+=(1,4.0)
    c+=(1,5.0)
    assert(c.size == 1)
  }

  "candSet " should " have size 0 when a reset has been called" in {

    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(1,2.0)
    c+=(1,3.0)
    c+=(1,4.0)
    c+=(1,5.0)
    c.reset
    assert(c.size == 0)
  }

  "candSet " should " have size 0 when a soft reset has been called" in {

    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(1,2.0)
    c+=(1,3.0)
    c+=(1,4.0)
    c+=(1,5.0)
    c.softReset
    assert(c.size == 0)
  }

  "candSet " should " have X candidates again after filling in X after a reset" in {

    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(2,2.0)
    c+=(3,3.0)
    c+=(4,4.0)
    c+=(5,5.0)
    c.reset
    c+=(1,1.0)
    c+=(2,2.0)
    c+=(3,3.0)
    c+=(4,4.0)
    c+=(5,5.0)
    assert(c.size == 5)
  }

  "candSet " should " have X candidates again after filling in X after a soft reset" in {

    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(2,2.0)
    c+=(3,3.0)
    c+=(4,4.0)
    c+=(5,5.0)
    c.softReset
    c.nonDistinctAdd(1,1.0)
    c.nonDistinctAdd(2,2.0)
    c.nonDistinctAdd(3,3.0)
    c.nonDistinctAdd(4,4.0)
    c.nonDistinctAdd(5,5.0)
    assert(c.size == 5)
  }

  "candSet " should "have correct candidates after doing a <= (filter) operation" in {
    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(2,2.0)
    c+=(5,5.0)
    c+=(3,3.0)
    c+=(4,4.0)
    c <= 3.0
    assert(c.size == 3)
    for(i <- 0 until c.size) {
      assert(c.ids.getInt(i) == 1 ||
      c.ids.getInt(i) == 2 ||
      c.ids.getInt(i) == 3)
    }
    assert(c.size == 3)
  }

  "candSet " should "return correct amount of candidates when using take" in {
    val c = new CandSet(10)
    c+=(1,1.0)
    c+=(2,2.0)
    c+=(3,3.0)
    c+=(4,4.0)
    c+=(5,5.0)
    c.take(3)
    assert(c.size == 3)
  }











}
