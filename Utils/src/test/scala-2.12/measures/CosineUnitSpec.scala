package measures

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class CosineUnitSpec extends FlatSpec with Matchers {
  "Cosine" should "return 0.0 distance when comparing two identical vectors" in {
    val distance = CosineUnit
    val rnd = new Random
    val vector1 = normalize(Array.fill[Float](128)(rnd.nextFloat))
    for(i <- 0 until 100)
      assert(distance.measure(vector1, vector1) == 0.0f)
  }

  def normalize(vec:Array[Float]) : Array[Float] = {
    val length = Math.sqrt(vec.map(x => x*x).sum)
    vec.map(x => (x / length).toFloat)
  }

}
