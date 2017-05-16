package lsh

import java.io.File

import actors.{DisaParserFacNumeric, HyperplaneFactory, RepetitionHandler}
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.Hyperplane
import io.Parser.{DisaParser, DisaParserNumeric}
import measures.EuclideanFast
import org.scalatest.{FlatSpec, Matchers}
import tools.CandSet

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHNumericDistributedSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {

      // Preparing tests

      val r = "akka.tcp://UnitTestSystem@127.0.0.1:2552/user/Repetition"


      val rnd = new Random(System.currentTimeMillis())
      val k = 12
      val dim = 128
      val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128), Hyperplane(k, rnd.nextLong, 128))
      val eucDataDir = "data/descriptors-40000-reduced-128-normalized.data"
      val dataSet = DisaParserNumeric(Source.fromFile(new File(eucDataDir)).getLines(), 128).toArray
      val system = ActorSystem("UnitTestSystem")
      val a1 = system.actorOf(Props[RepetitionHandler[Array[Float]]])
      val lsh = new LSHNumericDistributed(Array(a1))

      lsh.build(eucDataDir, 39290, DisaParserFacNumeric, hashFunctions.length, HyperplaneFactory, "twostep", 40000, k, dim, EuclideanFast, rnd.nextLong())
    }
  }


  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp._2,30)

      results(i) = res.size <= 30
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }


  "Query result (if not empty)" should "be of type CandSet" in {
    val f = fixture

    val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val res = f.lsh.query(qp._2,30)
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[(Int,Double)]()
    val t:CandSet = new CandSet(100)

    assert(res.getClass == t.getClass)

  }

  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp._2, 30)
      results(i) = res.size == res.distinct.size
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }
}
