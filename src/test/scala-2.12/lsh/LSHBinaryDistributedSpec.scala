package lsh

import java.io.File
import java.util

import actors._
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.{BitHash, Hyperplane}
import io.Parser.{DisaParserBinary, DisaParserNumeric}
import measures.{EuclideanFast, Hamming}
import org.apache.lucene.util.OpenBitSet
import org.scalatest.{FlatSpec, Matchers}
import tools.{CandSet, Tools}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHBinaryDistributedSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {

      // Preparing tests

      val rnd = new Random(54321)
      val k = 12
      val dim = 256
      //val hashFunctions = Array(BitHash(k, rnd.nextLong, dim), BitHash(k, rnd.nextLong, dim))
      val hashFunctions = Array(BitHash(k, rnd.nextLong, dim))
      val bitDataDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/ps-256-tiny.data"
      val eucDataDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/profiset-128-float-tiny.data"
      val bitQDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/ps-256-tiny.query"
      val eucQDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/profiset-128-float.query"
      val eucQueries = DisaParserNumeric(Source.fromFile(new File(eucQDir)).getLines(), 128).toArray
      val bitQueries = DisaParserBinary(Source.fromFile(new File(bitQDir)).getLines(), 256).toArray

      val system = ActorSystem("UnitTestSystem")
      val a1 = system.actorOf(Props[RepetitionHandler[OpenBitSet]])
      val a2 = system.actorOf(Props[RepetitionHandler[OpenBitSet]])
      val a3 = system.actorOf(Props[RepetitionHandler[OpenBitSet]])
      val a4 = system.actorOf(Props[RepetitionHandler[OpenBitSet]])
      val a5 = system.actorOf(Props[RepetitionHandler[OpenBitSet]])
      //val lsh = new LSHBinaryDistributed(Array(a1,a2,a3,a4,a5))
      val lsh = new LSHBinaryDistributed(Array(a1))

      lsh.build((bitDataDir, eucDataDir), 500000, DisaParserFacBitSet, hashFunctions.length, BitHashFactory, "twostep", 40000, k, dim, Hamming, rnd.nextLong())

    }
  }

/*
  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val qp = f.dataSetBit(f.rnd.nextInt(f.dataSet.length))
      val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val qpa = (qp._2, qpe._2, 2000)
      val res = f.lsh.query(qpa, 30)

      results(i) = res.size <= 30
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }


  "Query result (if not empty)" should "be of type Arraybuffer[(Int,double)]" in {
    val f = fixture

    val qp = f.dataSetBit(f.rnd.nextInt(f.dataSetBit.length))
    val qpe = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val qpa = (qp._2, qpe._2, 2000)
    val res = f.lsh.query(qpa, 30)
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[(Int,Double)]()
    val t:(Int,Double) = (1,2.0)
    if(res.nonEmpty) {
      assert(res(0).getClass == t.getClass)
    }

    assert(arr.getClass == res.getClass)
  }
*/
  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val index = f.rnd.nextInt(f.bitQueries.length)
      val qp = f.bitQueries(index)
      val qpe = f.eucQueries(index)
      val qpa = (qp._2, qpe._2, 5000) // 5000 will be searched by knn
      val res:CandSet = f.lsh.query(qpa, 50) // 50 will come out
      if(res.size != res.distinct.size) {
        println(res.size - res.distinct.size)
      }
      val i1 = new ArrayBuffer[Int]
      val i2 = new ArrayBuffer[Double]
      for(j <- 0 until res.size) {
        i1+=res.ids.getInt(j)
        i2 += math.sqrt(res.dists.getDouble(j))
      }
      println("avg query dists " + i2.sum / i2.size)

      results(i) = i1.size == i1.distinct.size
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }
}
