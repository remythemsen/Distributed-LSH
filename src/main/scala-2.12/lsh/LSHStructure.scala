package lsh

import messages.{FillTable, Query}

import scala.concurrent.{Await, Future}
import akka.actor._
import akka.util.Timeout
import scala.concurrent.duration._
import akka.pattern.ask

/**
  * Structure to be queried on for
  * approximate nearest neighbors
  * stored in its internal hashmaps
  */

class LSHStructure[A](repetitions:Array[ActorRef]) {

  /**
    * When Initializing LSH with a set of repetitions
    * Each repetition is reset, and rebuilt
    */

  build // builds the tables

  // TODO Change content in messages between nodes to be simple arrays instead of objects
  val resultSets:Array[Future[Any]] = new Array(repetitions.length)
  val statuses:Array[Future[Any]] = new Array(repetitions.length)
  implicit val timeout = Timeout(10.seconds)

  def query(qp:Array[Float], k:Int) : Array[Int] = {
    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < repetitions.length) {
      resultSets(i) = repetitions(i) ? Query(qp, k)
      i += 1
    }

    // Wait for all results to return
    // TODO Future sequence is a linear cost
    val res = Await.result(Future.sequence(resultSets), timeout.duration).asInstanceOf[Array[(Int, Double)]]
    res.sortBy(x => x._2).take(k).map(x => x._1)
  }

  def build : Unit = {
    var i = 0
    while(i < repetitions.length) {
      statuses(i) = repetitions(i) ? FillTable
      i += 1
    }

    // waiting for all tables to finish
    // TODO if all is successful, then return
    Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[Boolean]
  }
}
