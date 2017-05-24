package datastructures

import hashfunctions.{BitHash, Hyperplane}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.util.Random
import java.util
import java.util.concurrent.Executors

import scala.concurrent._
import scala.concurrent.duration._
import it.unimi.dsi.fastutil.ints.IntArrayList
import org.apache.lucene.util.OpenBitSet

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}

class ProbeTableSpec extends FlatSpec with Matchers {
  implicit val ec = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))
  val data:Array[Array[Float]] = Array.fill[Array[Float]](1)(new Array[Float](128))
  // TODO Fix this test
  val bitData:Array[OpenBitSet] = Array.fill[OpenBitSet](1)(new OpenBitSet)
  val arr = new Array[Float](128)
  val rnd = new Random(System.currentTimeMillis())

  "Memory Test" should "Consume some amount of mem :) " in {
    val l =  new IntArrayList(10)
    for(i <- 0 until 10) {
      l.add(i)
    }
    val r = new Array[Int](11)
    l.getElements(0, r, 0, 10)
    r
  }

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
