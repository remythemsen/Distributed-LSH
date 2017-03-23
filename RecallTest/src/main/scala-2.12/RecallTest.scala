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

  val repetitionAddresses = for {
    ip <- ips
    repetitionAddress <- {
      Array(systemName+ip+":"+repPort+actorPath)
    }
  } yield repetitionAddress

  val system = ActorSystem("RecallTestSystem")

  val lsh = new LSHStructure(for {
    address <- repetitionAddresses
    repetition <- Seq(system.actorSelection(address))
  } yield repetition)

  val rnd = new Random(System.currentTimeMillis())

  lsh.build("data/", () => Hyperplane(12, () => new Random(rnd.nextLong), 128),128,Euclidean)

}
