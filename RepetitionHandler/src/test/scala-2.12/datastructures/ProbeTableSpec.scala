package datastructures

import hashfunctions.{BitHash, Hyperplane}
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable
import scala.util.Random

class ProbeTableSpec extends FlatSpec with Matchers {

  val arr = new Array[Float](128)
  val rnd = new Random(System.currentTimeMillis())

  "Query " should "return results given a valid query" in {
    val k = 6
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(k, rnd.nextLong(), 128)
    val pt = new Table(hp)

    var i = 0

    val vec = (1,Array.fill[Float](128)(rnd.nextFloat))
    pt+=vec
    while(i < 100) {
      pt+=(rnd.nextInt,Array.fill[Float](128)(rnd.nextFloat))
      i+=1
    }

    assert(pt.query(hp(vec._2)).contains(vec._1)) // same object
  }

  "Query " should "return point inserted" in {
    for(i <- 0 until 1000) {
      val rnd = new Random
      val hf = BitHash(3, 2342980, 9)
      val pt = new Table(hf)
      val vec = (rnd.nextInt, mutable.BitSet(0,3,5,8))
      val vec1 = (rnd.nextInt, mutable.BitSet(1,2,5,7))
      val vec2 = (rnd.nextInt, mutable.BitSet(0,4,5,7))
      val vec3 = (rnd.nextInt, mutable.BitSet(1,2,2,6))
      val vec4 = (rnd.nextInt, mutable.BitSet(0,5,6,8))
      val vec5 = (rnd.nextInt, mutable.BitSet(1,2,3,6))
      pt+=vec
      pt+=vec1
      pt+=vec2
      pt+=vec3
      pt+=vec4
      pt+=vec5

      val res = pt.query(hf(vec._2))
      assert(res.nonEmpty)
    }

  }

  "Query " should "not throw exception given invalid query" in {
    val k = 6
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(k, rnd.nextLong(), 128)
    val vec = (1,Array.fill[Float](128)(rnd.nextFloat))
    val vec2 = (3,Array.fill[Float](128)(rnd.nextFloat))
    val pt = new Table(hp)
    pt+=vec
    assert(!pt.query(hp(vec2._2)).contains(vec2._1)) // same object
  }

}
