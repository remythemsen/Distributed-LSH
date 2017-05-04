package lsh

import java.io.File

import actors._
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.{BitHash, Hyperplane}
import io.Parser.{DisaParserBinary, DisaParserNumeric}
import measures.{Euclidean, Hamming}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHStructureBitSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {

      // Preparing tests

      val rnd = new Random(System.currentTimeMillis())
      val k = 16
      val dim = 256
      val hashFunctions = Array(BitHash(k, rnd.nextLong, dim))
      val bitDataDir = "data/descriptors-1-million-reduced-128-hamming-256bit.data"
      val eucDataDir = "data/descriptors-1-million-reduced-128-normalized.data"
      val dataSet = DisaParserNumeric(Source.fromFile(new File(eucDataDir)).getLines(), 128).take(50).toArray
      val dataSetBit = DisaParserBinary(Source.fromFile(new File(bitDataDir)).getLines(), 128).take(50).toArray
      val system = ActorSystem("UnitTestSystem")
      val a1 = system.actorOf(Props[RepetitionHandler[mutable.BitSet]])
      val lsh = new LSHStructure[mutable.BitSet](Array(a1))

      lsh.build(bitDataDir, eucDataDir, "binary", 1008263, DisaParserFacBitSet, 1, BitHashFactory, "twostep", 5000, k, dim, new Hamming(dim), rnd.nextLong())
    }
  }

  "Query result (if not empty)" should "be sorted ascending" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    for (i <- 0 until 50) {
      println("making q")
      val qp = f.dataSetBit(f.rnd.nextInt(f.dataSetBit.length))
      val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp,qpe,30).map(_._2)

      var isAsc = true
      var oldDist = 0.0
      for (x <- res) {
        if (oldDist > x) {
          isAsc = false
        } else {
          oldDist = x
        }
      }

      results(i) = isAsc
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val qp = f.dataSetBit(f.rnd.nextInt(f.dataSet.length))
      val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp,qpe, 30)

      results(i) = res.size <= 30
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }


  "Query result (if not empty)" should "be of type Arraybuffer[(Int,double,Int)]" in {
    val f = fixture

    val qp = f.dataSetBit(f.rnd.nextInt(f.dataSetBit.length))
    val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val res = f.lsh.query(qp,qpe, 30)
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[(Int,Double,Int)]()
    val t:(Int,Double,Int) = (1,2.0,2)
    if(res.nonEmpty) {
      assert(res(0).getClass == t.getClass)
    }

    assert(arr.getClass == res.getClass)
  }

  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSetBit(f.rnd.nextInt(f.dataSet.length))
      val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp, qpe, 30)
      results(i) = res.size == res.distinct.size
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }
}
