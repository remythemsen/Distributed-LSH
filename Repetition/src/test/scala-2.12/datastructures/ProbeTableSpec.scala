package datastructures

import hashfunctions.Hyperplane
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class ProbeTableSpec extends FlatSpec with Matchers {
  "Query " should "return results given a valid query" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val id = 1
    val tuple = (1, vec)

    val pt = new ProbeTable(hp)
    pt+=tuple

    assert(pt.query(vec).head == tuple) // same object
  }
}
