package lsh

import java.io.File

import actors.{BitHashFactory, DisaParserFac, HashFunctionFactory, RepetitionHandler, _}
import messages.{InitRepetition, Query, Stop}

import scala.concurrent.{Await, Future}
import akka.actor._
import akka.util.Timeout

import scala.concurrent.duration._
import akka.pattern.ask
import measures._
import akka.actor.{Address, AddressFromURIString, Deploy, Props}
import akka.remote.RemoteScope

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

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
  var distance:Distance[A] = _
  val repetitions:Array[ActorRef] = new Array[ActorRef](actorAdresses.length)
  var bit:Boolean = _
  var pq:mutable.PriorityQueue[(Int, Double, Int)] = _
  var eucDataSet:Array[(Int, Array[Float])] = _
  for(i <- actorAdresses.indices) {
    println("init'ing rephandler "+i+"!")
    this.repetitions(i) = system.actorOf(Props[RepetitionHandler[A]].withDeploy(Deploy(scope = RemoteScope(AddressFromURIString(actorAdresses(i))))))
  }

  // TODO Change content in messages between nodes to be simple arrays instead of objects
  val futureResults:Array[Future[Any]] = new Array(repetitions.length) // TODO Cannot use array in .sequence method, ... consider another method.
  implicit val timeout = Timeout(20.hours)
  import scala.concurrent.ExecutionContext.Implicits.global

  def query(qp:(Int, A), qpe:(Int, Array[Float]), k:Int) : ArrayBuffer[(Int, Double, Int)] = {
    //                                id, dist(qp), index
    val candidates = new ArrayBuffer[(Int,Double,Int)]()

    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < repetitions.length) {
      futureResults(i) = repetitions(i) ? Query(qp._2, k)
      i += 1
    }

    // Wait for all results to return
    var j = 0
    while(j < futureResults.length) {
      candidates ++= Await.result(futureResults(j), timeout.duration).asInstanceOf[ArrayBuffer[(Int, Double, Int)]]
      j+=1
    }

    val distinctCandidates = candidates.distinct

    // If the its bithash we need to do some extra work comparing points in euclidean space
    if(this.bit) {
      // Find actual nearest neighbors from candidates set in euclidean space
      var l = 0
      while(l < distinctCandidates.size) {
        // TODO Verify that euclidean is in fact better than cosineUnit here (97% vs 95% in tests so far)
        // distance of candidate l from qp in euclidean space
        val dist = Euclidean.measure(this.eucDataSet(distinctCandidates(l)._3)._2, qpe._2)
        if(pq.size < k) {
          this.pq.enqueue((distinctCandidates(l)._1, dist, distinctCandidates(l)._3))
        } else if (pq.head._2 > dist) {
          pq.dequeue()
          pq.enqueue((distinctCandidates(l)._1, dist, distinctCandidates(l)._3))
        }
        l+=1
      }
      val result:ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
      var m = 0
      while(m < k) {
        result+=pq.dequeue()
        m+=1
      }
      this.pq.clear
      result

    } else {
      distinctCandidates.sortBy(x => x._2).take(k)
    }

  }

  def build(filePath:String, eucFilePath:String, dataType:String, n:Int, parserFac:DisaParserFac[A], internalRepetitions:Int, hashFunctionFac:HashFunctionFactory[A], probeGenerator:String, maxCandsTotal:Int, functions:Int, dimensions:Int, simMeasure:Distance[A], seed:Long) : Boolean = {
    println("build was called!")
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(repetitions.length)
    var i = 0
    while(i < repetitions.length) {
      statuses += repetitions(i) ? InitRepetition(filePath, n, parserFac, internalRepetitions, hashFunctionFac, probeGenerator, maxCandsTotal/repetitions.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    // waiting for all tables to finish
    // TODO if all is successful, then return
    println("Done sending of build signals for repetitions")

    // If its bithashing, then we need an euclidean space dataset!
    this.eucDataSet = dataType match {
      case "binary" => {
        println("Loading Euclidean dataset!")
        this.bit = true

        implicit object Ord extends Ordering[(Int, Double, Int)] {
          def compare(x: (Int, Double, Int), y: (Int, Double, Int)) = x._2.compare(y._2)
        }

        this.distance = distance
        this.pq = new mutable.PriorityQueue[(Int, Double, Int)]
        DisaParserFacNumeric(eucFilePath, dimensions).toArray
      }
      case _ => {
        // We dont need euc dataset
        Array()
      }
    }

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    res.forall(x => x)
  }
  def destroy : Unit = {
    println("initiated shut down sequence..")
    var i = 0
    while(i < repetitions.length) {
      repetitions(i) ! Stop
      i += 1
    }
    println("Shutting down lsh...")
    this.system.terminate()
  }
}
