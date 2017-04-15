package actors

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import hashfunctions.Hyperplane
import measures.Euclidean
import messages.{InitRepetition, Query}
import org.scalatest.{FlatSpec, Matchers}
import multiprobing.{PQ, TwoStep, TwoStepSpec}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class RepetitionHandlerSpec extends FlatSpec with Matchers {


  "Query " should "return 0 or more results given a valid query" in {

    implicit val timeout = Timeout(10.hours)
    val rnd = new Random
    val system = ActorSystem("UnitTestSystem")
    val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
    val k = 6
    val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128))
    val ready = a1 ? InitRepetition("data/descriptors-40000-reduced-128.data",  39290,  hashFunctions.length, "hyperplane", "pq", 100,k, 128, Euclidean, rnd.nextLong)
    Await.result(ready, timeout.duration)
    val cands:Future[Any] = a1 ? Query(Array.fill[Float](128)(rnd.nextFloat), 30)
    val res = Await.result(cands, timeout.duration).asInstanceOf[Array[(Int, Double)]]

    assert(res != null)

    system.terminate()
  }

}
