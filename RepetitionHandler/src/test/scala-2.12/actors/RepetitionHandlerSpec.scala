package actors

import java.io.File

import IO.Parser.DisaParserNumeric
import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import hashfunctions.Hyperplane
import measures.{Cosine, CosineUnit, EuclideanFast}
import messages.{InitRepetition, Query}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.Random

class RepetitionHandlerSpec extends FlatSpec with Matchers {

  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {
      // Preparing tests
      val rnd = new Random(System.currentTimeMillis())
      val k = 8
      val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128), Hyperplane(k, rnd.nextLong(), 128), Hyperplane(k, rnd.nextLong(), 128), Hyperplane(k, rnd.nextLong(), 128))
      val system = ActorSystem("UnitTestSystem")
      val data = "C:\\datasets\\disa\\0\\descriptors-1-million-reduced-128-normalized.data"
      val a1 = system.actorOf(Props[actors.RepetitionHandler[Array[Float]]], name = "rep1")
      val queries = DisaParserNumeric(Source.fromFile(new File(data)).getLines(), 128).take(50).toArray
      // Populating repetition
      val ready = a1 ? InitRepetition(data, 1008263, DisaParserFacNumeric, DataSetFacNumeric, hashFunctions.length, HyperplaneFactory, "twostep", 100000, k, 128, EuclideanFast, rnd.nextLong)
      Await.result(ready, timeout.duration)
    }
  }

  "Query " should "return 0 or more results given any query" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    val res = Await.result(
      f.a1 ? Query(Array.fill[Float](128)(f.rnd.nextFloat), 30)
      , timeout.duration
    ).asInstanceOf[ArrayBuffer[(Int, Double)]]

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(res != null)
  }


  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.queries(f.rnd.nextInt(f.queries.length))
      val res = Await.result(
        f.a1 ? Query(qp._2, 30)
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

    val qp = f.queries(f.rnd.nextInt(f.queries.length))
    val res = Await.result(
      f.a1 ? Query(qp._2, 30)
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
