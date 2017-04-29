package measures

import java.io.File

import tools.Tools._
import io.Parser.DisaParser
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Random

class CosineSpec extends FlatSpec with Matchers {
  "Cosine" should "return 0.0 distance when comparing two identical vectors" in {
    val distance = Cosine
    val rnd = new Random
    val vector1 = Array.fill[Float](128)(rnd.nextFloat)
    for(i <- 0 until 100)
      assert(distance.measure(vector1, vector1) == 0.0)
  }
}
