package datastructures

import hashfunctions.{BitHash, Hyperplane}
import org.scalatest.{FlatSpec, Matchers}
import scala.collection.mutable
import scala.util.Random

class ProbeTableSpec extends FlatSpec with Matchers {

  val data:Array[Array[Float]] = Array.fill[Array[Float]](1)(new Array[Float](128))
  // TODO Fix this test
  val bitData:Array[mutable.BitSet] = Array.fill[mutable.BitSet](1)(new mutable.BitSet)
  val arr = new Array[Float](128)
  val rnd = new Random(System.currentTimeMillis())

  "Query " should "return results given a valid query" in {
    val k = 6
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(k, rnd.nextLong(), 128)
    val pt = new Table(hp, data)

    var i = 0

    val vec = 1
    pt+=vec
    while(i < 100) {
      pt+=rnd.nextInt(data.length)
      i+=1
    }

    assert(pt.query(hp(data(vec))).contains(vec)) // same object
  }

  "Query " should "return point inserted" in {
    for(i <- 0 until 1000) {
      val rnd = new Random
      val hf = BitHash(3, 2342980, 9)
      val pt = new Table(hf, bitData)
      val vec = (rnd.nextInt)
      val vec1 = (rnd.nextInt)
      val vec2 = (rnd.nextInt)
      val vec3 = (rnd.nextInt)
      val vec4 = (rnd.nextInt)
      val vec5 = (rnd.nextInt)
      pt+=vec
      pt+=vec1
      pt+=vec2
      pt+=vec3
      pt+=vec4
      pt+=vec5

      val res = pt.query(hf(bitData(vec)))
    }
    assert(false)


  }

/*  "Query " should "not throw exception given invalid query" in {
    val k = 6
    val rnd = new Random(System.currentTimeMillis())
    val hp = Hyperplane(k, rnd.nextLong(), 128)
    val vec = (1,Array.fill[Float](128)(rnd.nextFloat))
    val vec2 = (3,Array.fill[Float](128)(rnd.nextFloat))
    val pt = new Table(hp)
    pt+=vec
    assert(!pt.query(hp(vec2._2)).contains(vec2._1)) // same object
  }*/

}
