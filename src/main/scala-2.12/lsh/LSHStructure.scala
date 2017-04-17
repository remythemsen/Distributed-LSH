package lsh

import actors.{HashFunctionFactory, RepetitionHandler}
import messages.{InitRepetition, Query}

import scala.concurrent.{Await, Future}
import akka.actor._
import akka.util.Timeout

import scala.concurrent.duration._
import akka.pattern.ask
import measures.Distance
import akka.actor.{Address, AddressFromURIString, Deploy, Props}
import akka.remote.RemoteScope

import scala.collection.mutable.ArrayBuffer

/**
  * Structure to be queried on for
  * approximate nearest neighbors
  * stored in its internal hashmaps
  */

class LSHStructure[A](actorAdresses:Array[String]) {

  /**
    * When Initializing LSH with a set of repetitions
    * Each repetition is reset, and rebuilt
    */

  val system = ActorSystem("LSHSystem")
  println("System started")
  // Start RepetitionHandler actors
  val repetitions:Array[ActorRef] = new Array[ActorRef](actorAdresses.length)
  for(i <- actorAdresses.indices) {
    this.repetitions(i) = system.actorOf(Props[RepetitionHandler[A]].withDeploy(Deploy(scope = RemoteScope(AddressFromURIString(actorAdresses(i))))))
  }

  // TODO Change content in messages between nodes to be simple arrays instead of objects
  val futureResults:Array[Future[Any]] = new Array(repetitions.length) // TODO Cannot use array in .sequence method, ... consider another method.
  implicit val timeout = Timeout(20.hours)
  import scala.concurrent.ExecutionContext.Implicits.global

  def query(qp:(Int, A), k:Int) : ArrayBuffer[Int] = {
    val candidates = new ArrayBuffer[(Int,Double)]()

    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < repetitions.length) {
      futureResults(i) = repetitions(i) ? Query(qp._2, k)
      i += 1
    }

    // Wait for all results to return
    var j = 0
    while(j < futureResults.length) {
      candidates ++= Await.result(futureResults(j), timeout.duration).asInstanceOf[ArrayBuffer[(Int, Double)]]
      j+=1
    }

    candidates.distinct.sortBy(x => x._2).take(k).map(x => x._1)
  }

  def build(filePath:String, n:Int, internalRepetitions:Int, hashFunctionFac:HashFunctionFactory[A], probeGenerator:String, maxCandsTotal:Int, functions:Int, dimensions:Int, simMeasure:Distance, seed:Long) : Boolean = {
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(repetitions.length)
    var i = 0
    while(i < repetitions.length) {
      statuses += repetitions(i) ? InitRepetition(filePath, n, internalRepetitions, hashFunctionFac, probeGenerator, maxCandsTotal/repetitions.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    // waiting for all tables to finish
    // TODO if all is successful, then return
    println("Done sending of build signals for repetitions")
    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    res.forall(x => x)
  }
}
