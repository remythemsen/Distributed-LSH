package hashfunctions

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 23-03-2017.
  */
class HyperplaneLongSpec extends FlatSpec with Matchers {
  "Apply method" should "return a long" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new HyperplaneLong(6, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }

  "Apply method" should "return correct result" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new HyperplaneLong(1, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val l:Long = 0
    val r = hp(vec)
    assert(r.getClass == l.getClass)
  }

  "generateProbes method" should "generate correct array" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new HyperplaneLong(3, rnd.nextLong(), 128)

    val key:Long = 4

    val pbs = hp.generateProbes(key)
    val correct = Array(key,5,7,1,6,2,0)
    val values = pbs.zipWithIndex
    assert(values.forall((x) => x._1 == correct(x._2)))

  }

  "generateProbes method" should "generate an array of longs" in {
    val rnd = new Random(System.currentTimeMillis())
    val hp = new HyperplaneLong(10, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val pbs = hp.generateProbes(hp(vec))
    assert(pbs.getClass == new Array[Long](6).getClass)
  }

  "generateProbes method" should "make set of 1+(k*(k+1)/2)" in {
    val k = 10
    val rnd = new Random(System.currentTimeMillis())
    val hp = new HyperplaneLong(k, rnd.nextLong(), 128)
    val vec = Array.fill[Float](128)(rnd.nextFloat)
    val pbs = hp.generateProbes(hp(vec))
    assert(pbs.length == (k*(k+1)/2)+1)
  }


}
