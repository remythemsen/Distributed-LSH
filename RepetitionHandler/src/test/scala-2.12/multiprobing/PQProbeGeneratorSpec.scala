package multiprobing

import hashfunctions.HyperplaneLong
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

/**
  * Created by remeeh on 07-04-2017.
  */
class PQProbeGeneratorSpec extends FlatSpec with Matchers {
  "generate" should "make correct probeset on simple input" in {
    val k = 2
    val rnd = new Random(System.currentTimeMillis())
    val gen = new PQProbeGenerator(k, Array(new HyperplaneLong(k,rnd.nextLong,128))) // k=2 L=1
    gen.generate(Array.fill[Float](128)(rnd.nextFloat()))
    val res = gen.map(x => x._2).toArray
    assert(res.contains(0l))
    assert(res.contains(1l))
    assert(res.contains(2l))
    assert(res.contains(3l))

    println("done")
  }

  "generate" should "make correct probeset on simple input with 2 keys" in {
    val k = 2
    val rnd = new Random(System.currentTimeMillis())
    val gen = new PQProbeGenerator(k, {
      Array(new HyperplaneLong(k,rnd.nextLong,128),new HyperplaneLong(k,rnd.nextLong,128))
    }) // k=2 L=1
    gen.generate(Array.fill[Float](128)(rnd.nextFloat()))
    val res = gen.toArray
    val res1 = res.filter(x=>x._1 ==0).map(x => x._2)
    val res2 = res.filter(x=>x._1 ==1).map(x => x._2)
    assert(res1.contains(0l))
    assert(res1.contains(1l))
    assert(res1.contains(2l))
    assert(res1.contains(3l))

    assert(res2.contains(0l))
    assert(res2.contains(1l))
    assert(res2.contains(2l))
    assert(res2.contains(3l))
    println("done")
  }
}
