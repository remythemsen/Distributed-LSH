package measures

import org.scalatest.{FlatSpec, Matchers}

class EuclideanSpec extends FlatSpec with Matchers {
  "measure" should "get correct result" in {
    val v1 = Array(3.12f, 4.122f, -2.21f, 0.0012f)
    val v2 = Array(0.0021f, 2.12f, -0.0021f, 0.0005f)
    val result = Euclidean.measure(v1, v2)
    val result2 = Euclidean.measure(v2, v1)
    assert((Math.round(result*100000.0f)/100000.0f) == 4.31325f)
    assert((Math.round(result2*100000.0f)/100000.0f) == 4.31325f)
  }



}
