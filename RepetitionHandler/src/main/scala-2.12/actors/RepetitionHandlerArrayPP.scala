package actors

import java.io.File

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import datastructures.ProbeTableLongMapOld
import hashfunctions.{Crosspolytope, Hyperplane, HyperplaneLong}
import io.Parser.DisaParser
import measures.Distance
import messages._
import tools.{SQuickSelect, SVQuickSelect}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

object Program2 extends App {
  val system = ActorSystem("RepetitionSystem")
  val repetitionHandler = system.actorOf(Props[RepetitionHandlerArrayPP], name = "Repetition")
}

class RepetitionHandlerArrayPP extends Actor {

  // Set of internal repetitions
  private var repetitions:Array[ProbeTableLongMapOld] = _
  private var simMeasure:Distance = _

  // Internal lookup map for vectors in datastructure
  private var dataSet:Array[(Int, Array[Float])] = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetition(buildFromFile, n, internalReps, hashFunction, functions, dimensions, distance, seed) => {

      this.simMeasure = distance
      this.dataSet = new Array(n)
      val rnd = new Random(seed)

      // Loading in dataset
      println("Loading dataset...")
      val parser = DisaParser(Source.fromFile(new File(buildFromFile)).getLines(), dimensions)
      var c = 0
      while (parser.hasNext) {
        if (c % 100 == 0) println(c * 100 / n)
        this.dataSet(c) = parser.next
        c += 1
      }

      // Initializing internal repetitions
      println("Initializing repetitions...")
      this.repetitions = new Array(internalReps)
      val futures: Array[Future[Any]] = new Array(internalReps)

      //var i = 0
      for (i <- 0 until internalReps) {
        this.repetitions(i) = new ProbeTableLongMapOld({
          hashFunction.toLowerCase() match {
            case "hyperplane" => new Hyperplane(functions, rnd.nextLong(), dimensions)
            //case "crosspolytope" => new Crosspolytope(functions, rnd.nextLong(), dimensions)
          }
        }, functions*30)

        futures(i) = Future {
          buildRepetition(i, n)
        }
      }

      implicit val timeout = Timeout(20.hours)
      Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)


      sender ! true
    }

    case Query(qp, k) => { // Returns Array[(Int,Double)]
      var candidates: Array[Int] = new Array(0)

      // Getting candidates
      var i = 0
      while (i < this.repetitions.length) {
        candidates = candidates ++ this.repetitions(i).query(qp)
        i += 1
      }
      candidates = candidates.distinct
      val results:Array[(Int, Double)] = new Array(candidates.length)


      var j = 0
      while(j < candidates.length) {
        results(j) = (candidates(j), this.simMeasure.measure(dataSet(candidates(j))._2, qp))
        j += 1
      }

      //val results = candidates.map(x => (x, this.simMeasure.measure(dataSet(x)._2, qp)))

      // TODO Check correctness of k-1
      // TODO Find different version of quickselect
      val kthDist = SQuickSelect.quickSelect(results, {
        if (results.length < k) results.length - 1
        else k - 1
      })

      // TODO Dont use built in filter
      sender ! results.filter(x => x._2 <= kthDist)
    }
  }

  def buildRepetition(mapRef:Int, dataSize:Int):Boolean = {
    var j = 0
    while (j < this.dataSet.length) {
      // Insert key value pair of key generated from vector, and value: Index in dataSet
      if (j % 100 == 0) println(j * 100 / dataSize)
      this.repetitions(mapRef) += (this.dataSet(j), j)
      j += 1
    }
    true
  }
}
