package actors

import java.io.File
import java.util

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import com.googlecode.javaewah.datastructure.BitSet
import hashfunctions.{BitHash, Hyperplane}
import io.Parser.{DisaParserBinary, DisaParserNumeric}
import measures.{EuclideanFast, Hamming}
import messages.{InitRepetition, Query}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

class RepetitionHandlerBitSpec extends FlatSpec with Matchers {

  implicit val timeout = Timeout(10.hours)

  def randomBitSet(dim:Int, seed:Long):BitSet = {
    val rnd = new Random(seed)
    var i = 0
    var res = new BitSet
    while(i < dim) {
      if(rnd.nextBoolean()) {
        res.set(i)
      }
      i+=1
    }
    res
  }

  def fixture = {
    new {
      // Preparing tests
      val rnd = new Random(System.currentTimeMillis())
      val data = "D:\\datasets\\disa\\0\\descriptors-1-million-reduced-128-hamming-256bit.data"
      val k = 2
      val maxCands = 10000
      val n = 1008263
      val dimensions = 256
      val hashFunctions = Array(BitHash(k, rnd.nextLong, dimensions), BitHash(k, rnd.nextLong(), dimensions))
      val system = ActorSystem("UnitTestSystem")
      val a1 = system.actorOf(Props[actors.RepetitionHandler[BitSet]], name = "rep1")
      println("making dataset")
      //val dataSet = DisaParserBinary(Source.fromFile(new File(data)).getLines(), dimensions)

      // Populating repetition
/*      val ready = a1 ? InitRepetition(data, n, DisaParserFacBitSet, DataSetBitSet, hashFunctions.length, BitHashFactory, "twostep", maxCands, k, dimensions, new Hamming(dimensions), rnd.nextLong)
      Await.result(ready, timeout.duration)*/
    }
  }


  "Query " should "return 0 or more results given any query" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    val res = Await.result(
      f.a1 ? Query(randomBitSet(f.dimensions, f.rnd.nextLong()), 30)
      , timeout.duration
    ).asInstanceOf[ArrayBuffer[(Int, Double, Int)]]

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(res != null)
  }



  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = randomBitSet(f.dimensions, f.rnd.nextLong())
      val res = Await.result(
        f.a1 ? Query(qp, 30)
        , timeout.duration
      ).asInstanceOf[ArrayBuffer[(Int, Double, Int)]].map(x => x._1)

      results(i) = res.size == res.distinct.size

    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "be of type Arraybuffer[(Int, Double, Int)]" in {
    val f = fixture


    val qp = randomBitSet(f.dimensions, f.rnd.nextLong())
    val res = Await.result(
      f.a1 ? Query(qp, 30)
      , timeout.duration
    ).asInstanceOf[ArrayBuffer[(Int, Double, Int)]]

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[(Int, Double, Int)]()
    val d:Double = 0.0
    val i:Int = 0
    if(res.nonEmpty) {
      assert(res(0)._2.getClass == d.getClass)
      assert(res(0)._1.getClass == i.getClass)
    }

    assert(arr.getClass == res.getClass)
  }


}
