import java.io.File

import akka.actor.ActorSystem
import hashfunctions.Hyperplane
import io.Parser
import io.Parser.DisaParser
import lsh.LSHStructure
import measures.Euclidean

import scala.collection.mutable.ArrayBuffer
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
  println("System started")

  val lsh = new LSHStructure(for {
    address <- repetitionAddresses
    repetition <- Seq(system.actorSelection(address))
  } yield repetition)
  println("Structure initialized")

  val rnd = new Random(System.currentTimeMillis())

  if(lsh.build("data/descriptors-40000-reduced-128.data", 39290, 1, "Hyperplane", "pq", 1000, 12, 128,Euclidean, rnd.nextLong())) {
    println("Structure Was built!")
    val queryPoints = DisaParser(Source.fromFile(new File("data/descriptors-40000-reduced-128.queries")).getLines(), 128).toArray

    for(qp <- queryPoints) {
      val result:ArrayBuffer[Int] = lsh.query(qp._2, 10)
      println("query done")
    }
  }else {
    println("structure did not build successfully :(")
  }
  def timer[R](r: => R): Unit = {

  }

}
