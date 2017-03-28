package datastructures

import hashfunctions.Hyperplane
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class ProbeTableLongMapSpec extends FlatSpec with Matchers {
  "Query " should "return results given a valid query" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(6, rnd.nextLong(), 128)
    val pt = new ProbeTableLongMap(hp)

    var i = 0

    val vec = (1,Array.fill[Float](128)(rnd.nextFloat))
    pt+=vec
    while(i < 100) {
      pt+=(rnd.nextInt,Array.fill[Float](128)(rnd.nextFloat))
      i+=1
    }

    assert(pt.query(vec._2).contains(vec)) // same object

  }
  "Query " should "not throw exception given invalid query" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new Hyperplane(6, rnd.nextLong(), 128)
    val vec = (1,Array.fill[Float](128)(rnd.nextFloat))
    val vec2 = (3,Array.fill[Float](128)(rnd.nextFloat))
    val pt = new ProbeTableLongMap(hp)
    pt+=vec
    assert(!pt.query(vec2._2).contains(vec2)) // same object
  }

}
