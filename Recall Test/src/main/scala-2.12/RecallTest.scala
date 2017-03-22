import actors.Repetition
import akka.actor.ActorSystem
import hashfunctions.Hyperplane
import lsh.LSHStructure
import measures.Euclidean

import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 22-03-2017.
  */
object RecallTest extends App {

  // Remote Repetition references:
  val ips = Source.fromFile("data/ips").getLines().next.split(" ") // Ip's of tablehandlers
  val repPort = 2552
  val repSystemName = "RepetitionSystem" // table handler Actor systemname
  val systemName = "akka.tcp://"+repSystemName+"@"
  val actorPath = "/user/Repetition"

  val repetitions = for {
    ip <- ips
    tableHandlerAddress <- {
      Array(systemName+ip+":"+repPort+actorPath)
    }
  } yield tableHandlerAddress


  val rnd = new Random(System.currentTimeMillis())

  //val lsh = new LSHStructure(repetitions, () => Hyperplane(12, () => new Random(rnd.nextLong), 128),Euclidean)

}
