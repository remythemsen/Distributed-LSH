package actors

import akka.actor.{ActorSystem, Props}
import measures.Euclidean
import org.scalatest.{FlatSpec, Matchers}
import messages.{InitRepetition, Query}

import scala.concurrent.{Await, Future}
import akka.util.Timeout

import scala.concurrent.duration._
import akka.pattern.ask
import scala.util.Random

class RepetitionHandlerSpec extends FlatSpec with Matchers {


  "Query " should "return 0 or more results given a valid query" in {

    implicit val timeout = Timeout(10.hours)
    val rnd = new Random
    val system = ActorSystem("BenchmarkSystem")
    val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
    val ready = a1 ? InitRepetition("data/descriptors-40000-reduced-128.data",  39290,  1, "hyperplane", 12, 128, Euclidean, rnd.nextLong)
    Await.result(ready, timeout.duration)
    val cands:Future[Any] = a1 ? Query(Array.fill[Float](128)(rnd.nextFloat), 30)
    val res = Await.result(cands, timeout.duration).asInstanceOf[Array[(Int, Double)]]

    assert(res != null)

    system.terminate()
  }

}
