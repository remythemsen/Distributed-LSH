package actors

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import hashfunctions.Hyperplane
import io.Parser.DisaParser
import measures.Euclidean
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
      val k = 4
      val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128), Hyperplane(k, rnd.nextLong(), 128))
      val system = ActorSystem("UnitTestSystem")
      val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
      val dataSet = DisaParser(Source.fromFile(new File("data/descriptors-40000-reduced-128.data")).getLines(), 128).toArray
      // Populating repetition
      val ready = a1 ? InitRepetition("data/descriptors-40000-reduced-128.data", 39290, hashFunctions.length, "hyperplane", "pq", 1000, k, 128, Euclidean, rnd.nextLong)
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

  "Query result (if not empty)" should "not contain query point itself" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = Await.result(
        f.a1 ? Query(qp._2, 30)
        , timeout.duration
      ).asInstanceOf[ArrayBuffer[(Int,Double)]]
      results(i) = !res.contains(qp._1)
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = Await.result(
        f.a1 ? Query(qp._2, 30)
        , timeout.duration
      ).asInstanceOf[ArrayBuffer[(Int, Double)]].map(x => x._1)

      results(i) = res.size == res.distinct.size
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "be of type Arraybuffer[(Int, Double)]" in {
    val f = fixture

    val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val res = Await.result(
      f.a1 ? Query(qp._2, 30)
      , timeout.duration
    ).asInstanceOf[ArrayBuffer[(Int, Double)]]

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[(Int, Double)]()
    val d:Double = 0.0
    val i:Int = 0
    if(res.nonEmpty) {
      assert(res(0)._2.getClass == d.getClass)
      assert(res(0)._1.getClass == i.getClass)
    }

    assert(arr.getClass == res.getClass)
  }


}
