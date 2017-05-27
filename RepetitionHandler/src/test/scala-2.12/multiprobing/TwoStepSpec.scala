package multiprobing

import hashfunctions.Hyperplane
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TwoStepSpec extends FlatSpec with Matchers {
  def fixture = {
    new {
      val k = 2
      val rnd = new Random(System.currentTimeMillis())
      val hf = Hyperplane(k,rnd.nextLong,128)
      val hf2 = Hyperplane(k,rnd.nextLong,128)
      val gen = new TwoStep[Array[Float]](k, Array(hf,hf2)) // k=2 L=1
    }
  }

  "first two probes to come out" should " be the keys of hf and hf2" in {
    val f = fixture
    for(i <- 0 until 500) {
      val v = Array.fill[Float](128)(f.rnd.nextFloat())
      f.gen.generate(v)
      val one = f.gen.next()
      val two = f.gen.next()
      assert(f.hf(v) == one._2)
      assert(f.hf2(v) == two._2)
    }




  }

  "generate" should "make correct probeset on simple input" in {
    val f = fixture
    f.gen.generate(Array.fill[Float](128)(f.rnd.nextFloat()))
    val res = f.gen.map(x => x._2).toArray
    assert(res.contains(0l))
    assert(res.contains(1l))
    assert(res.contains(2l))
    assert(res.contains(3l))
  }

  "generate" should "make correct probeset on simple input with 2 keys" in {
    val f = fixture
    val gen = new PQ[Array[Float]](f.k, {
      Array(Hyperplane(f.k,f.rnd.nextLong,128),Hyperplane(f.k,f.rnd.nextLong,128))
    }) // k=2 L=1
    gen.generate(Array.fill[Float](128)(f.rnd.nextFloat()))
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
  }


  "generate" should "generate the correct amount of keys" in {
    val f = fixture
    val v = Array.fill[Float](128)(f.rnd.nextFloat())
    f.gen.generate(v)

    assert(f.gen.toArray.length == 8)
  }

  "generate" should "generate the input key itself" in {
    val f = fixture


    val hf = Hyperplane(12, f.rnd.nextLong,128)
    val gen = new TwoStep[Array[Float]](12, Array(hf)) // k=2 L=1
    val v = Array.fill[Float](128)(f.rnd.nextFloat())
    val r = hf(v)
    gen.generate(v)
    val res = gen.toArray.map(_._2)
    assert(res.contains(r))
  }

  "generate" should "out put input key itself first" in {
    val f = fixture
    val hf = Hyperplane(12, f.rnd.nextLong,128)
    val gen = new TwoStep[Array[Float]](12, Array(hf)) // k=2 L=1
    val v = Array.fill[Float](128)(f.rnd.nextFloat())
    val r = hf(v)
    gen.generate(v)

    val ab = new ArrayBuffer[(Int, Long)]()
    while(gen.hasNext()) {
      ab += gen.next()
    }
    val res = ab.head._2
    assert(res == r)
  }
}
