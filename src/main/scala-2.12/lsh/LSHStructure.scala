package lsh

import actors.Repetition
import messages.{Query, QueryResult}

import scala.concurrent.{Await, Future}
import akka.dispatch.Await
import akka.dispatch.Future
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._

/**
  * Structure to be queried on for
  * approximate nearest neighbors
  * stored in its internal hashmaps
  */

class LSHStructure[A](repetitions:Array[Repetition]) {

  // Check if reps are ready,
  // then return structure
  // else send init sig and wait

  // TODO Change content in messages between nodes to be simple arrays instead of objects
  val resultSets:Array[Future[QueryResult]] = new Array(repetitions.length)

  def query(qp:Array[Float], k:Int) : Array[Int] = {
    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < repetitions.length) {
      resultSets(i) = repetitions(i) ? Query(qp, k)
      i += 0
    }

    // Wait for all results to return
    Await.result(future, timeout.duration).asInstanceOf[Int, Double])]
  }
}
