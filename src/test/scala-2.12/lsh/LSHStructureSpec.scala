package lsh

import akka.actor.{ActorSystem, Props}
import measures.Euclidean
import org.scalatest.{FlatSpec, Matchers}
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHStructureSpec extends FlatSpec with Matchers {
  "LSH Structure" should "return set of sorted candidates" in {
    val system = ActorSystem("BenchmarkSystem")
    val rnd = new Random(System.currentTimeMillis())
    val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
    //val a2 = system.actorOf(Props[actors.Repetition], name = "rep2")
    val lsh = new LSHStructure(Array(system.actorSelection(a1.path)/**, system.actorSelection(a2.path)**/))
    lsh.build("data/descriptors-40000-reduced-128.data",39290, 1, "Hyperplane", 14, 128,Euclidean, rnd.nextLong())
    println("Structure Was built!")
    val qp = Array.fill[Float](128)(rnd.nextFloat())//queryPoints.next

    lsh.query(qp, 10)

  }
}
