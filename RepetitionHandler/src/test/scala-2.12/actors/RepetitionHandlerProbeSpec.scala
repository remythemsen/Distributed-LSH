package actors

import akka.actor.{ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout
import hashfunctions.HyperplaneLong
import measures.Euclidean
import messages.{InitRepetition, InitRepetitionProbe, Query}
import org.scalatest.{FlatSpec, Matchers}
import multiprobing.{PQProbeGenerator, TwoStepProbeGenerator, TwoStepProbeGeneratorSpec}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Random

class RepetitionHandlerProbeSpec extends FlatSpec with Matchers {


  "Query " should "return 0 or more results given a valid query" in {

    implicit val timeout = Timeout(10.hours)
    val rnd = new Random
    val system = ActorSystem("UnitTestSystem")
    val a1 = system.actorOf(Props[actors.RepetitionHandlerProbe], name = "rep1")
    val k = 6
    val hashFunctions = Array(new HyperplaneLong(k, rnd.nextLong, 128))
    val ready = a1 ? InitRepetitionProbe("data/descriptors-40000-reduced-128.data",  39290,  hashFunctions.length, "hyperplane", new PQProbeGenerator(k, hashFunctions), 100,k, 128, Euclidean, rnd.nextLong)
    Await.result(ready, timeout.duration)
    val cands:Future[Any] = a1 ? Query(Array.fill[Float](128)(rnd.nextFloat), 30)
    val res = Await.result(cands, timeout.duration).asInstanceOf[Array[(Int, Double)]]

    assert(res != null)

    system.terminate()
  }

}
