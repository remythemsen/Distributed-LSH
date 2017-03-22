package actors

import java.io.File

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import datastructures.ProbeTable
import hashfunctions.HashFunction
import io.Parser
import io.Parser.DisaParser
import measures.Distance
import messages._
import tools.QuickSelect

object Program extends App {
  val system = ActorSystem("RepetitionSystem")
  val repetition = system.actorOf(Props[Repetition], name = "Repetition")
}

class Repetition(hashFunction: () => HashFunction, distance:Distance) extends Actor {

  private var table:ProbeTable = _

  override def receive: Receive = {
    // TODO Move file parsing out of class
    case FillTable(buildFromFile, dimensions) =>
      this.table = new ProbeTable(hashFunction)
      val parser = DisaParser(Parser.memMappedIterator(new File(buildFromFile)), dimensions)
      while (parser.hasNext) {
        this.table += parser.next
      }

      sender ! true

    case Query(vec, k) =>
      // case query, look in internal table, and get top 30.
      sender ! { // TODO measure times, return to sender (
        val candidates = this.table.query(vec)
        val cWithDistance = candidates.map(x => (x._1, distance.measure(x._2, vec)))

        // TODO Check correctness of k-1
        val kthDist = QuickSelect.quickSelect(cWithDistance, {
          if(cWithDistance.length < k) cWithDistance.size-1
          else k-1
        })._2

        // TODO Make sure we dont send arraybuffer
        sender ! cWithDistance.filter(x => x._2 <= kthDist)

      }

  }
}
