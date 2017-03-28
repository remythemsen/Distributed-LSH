package actors

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import datastructures.{ProbeTable, ProbeTableLongMap}
import hashfunctions.{Crosspolytope, HashFunction, Hyperplane}
import io.Parser
import io.Parser.DisaParser
import measures.Distance
import messages._
import tools.{QuickSelect, SQuickSelect, SSQuickSelect}

import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Program extends App {
  val system = ActorSystem("RepetitionSystem")
  val repetition = system.actorOf(Props[Repetition], name = "Repetition")
}

class Repetition extends Actor {

  private var table:ProbeTableLongMap = _
  private var simMeasure:Distance = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetition(buildFromFile, hashFunction, functions, dimensions, distance, seed) =>
      this.table = new ProbeTableLongMap({
        hashFunction.toLowerCase() match {
          case "hyperplane" => new Hyperplane(functions, seed, dimensions)
          case "crosspolytope" => new Crosspolytope(functions, seed, dimensions)
        }
      })
      this.simMeasure = distance
      val parser = DisaParser(Source.fromFile(new File(buildFromFile)).getLines(), dimensions)
      var c = 0
      while (parser.hasNext) {
        println(c * 100 / 39290) // TODO Remove this!!!
        this.table += parser.next
        c+=1
      }

      sender ! true

    case Query(vec, k) =>
      // case query, look in internal table, and get top 30.
      sender ! { // TODO measure times, return to sender (
        val candidates:ArrayBuffer[(Int, Array[Float])] = this.table.query(vec)
        val resultPairs:Array[(Int, Double)] = new Array[(Int, Double)](candidates.length)
        var i = 0
        while(i < candidates.length) {
          resultPairs(i) = (candidates(i)._1, this.simMeasure.measure(vec, candidates(i)._2))
          i+=1
        }

        // TODO Check correctness of k-1
        // TODO Find different version of quickselect
        val kthDist = SQuickSelect.quickSelect(resultPairs, {
          if(resultPairs.length < k) resultPairs.length-1
          else k-1
        })

        // TODO Dont use built in filter
        sender ! resultPairs.filter(x => x._2 <= kthDist)

      }

  }
}
