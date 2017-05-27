package multiprobing

import hashfunctions.Hyperplane
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class PQSpec extends FlatSpec with Matchers {

  def fixture = {
    new {
      val k = 2
      val rnd = new Random(System.currentTimeMillis())
      val hf = Hyperplane(k,rnd.nextLong,128)
      val hf2 = Hyperplane(k,rnd.nextLong,128)
      val gen = new PQ[Array[Float]](k, Array(hf,hf2)) // k=2 L=1
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

  "generate" should "release keys in correct priority" in {
    val f = fixture
    val v = Array.fill[Float](128)(f.rnd.nextFloat())
    f.gen.generate(v)
    val key = f.hf(v)
    val key2 = f.hf2(v)
    val firstPrio = f.gen.next() // should be the input vectors 'key' from hf
    val secondPrio = f.gen.next() // should be the input vectors 'key' from hf2
    val thirdPrio = f.gen.next()


    // The next should be # of 1-step probes
    def hasOneBitDifferent(orgKey:Long, newKey:Long): Boolean = {
      var i = 0
      var c = 0
      while(i < f.k) {
        if((orgKey & (1<<i)) != (newKey & (1<<i))){
          c+=1
        }
        i += 1
      }
      c == 1
    }

    assert(firstPrio._2.equals(key))
    assert(secondPrio._2.equals(key2))
    assert(hasOneBitDifferent(key, thirdPrio._2) || hasOneBitDifferent(key2, thirdPrio._2))

  }

  "generate" should "generate the correct amount of keys" in {
    val f = fixture
    val v = Array.fill[Float](128)(f.rnd.nextFloat())
    f.gen.generate(v)

    assert(f.gen.toArray.length == 8)
  }

}
