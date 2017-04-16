package lsh

import java.io.File

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.Hyperplane
import io.Parser.DisaParser
import measures.Euclidean
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.Await
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHStructureSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {
      // Preparing tests
      val rnd = new Random(System.currentTimeMillis())
      val k = 4
      val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128))
      val system = ActorSystem("UnitTestSystem")
      val dataSet = DisaParser(Source.fromFile(new File("data/descriptors-40000-reduced-128.data")).getLines(), 128).toArray
      val a1 = system.actorOf(Props[actors.RepetitionHandler], name = "rep1")
      val lsh = new LSHStructure(Array(system.actorSelection(a1.path)

        /** , system.actorSelection(a2.path) **/))
      lsh.build("data/descriptors-40000-reduced-128.data", 39290, 1, "Hyperplane", "pq", 1000, k, 128, Euclidean, rnd.nextLong())
    }
  }

  "Query result (if not empty)" should "be sorted ascending" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    for (i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp, 30).map(x => Euclidean.measure(qp._2, f.dataSet(x)._2))

      var isAsc = true
      var oldDist = 0.0
      for (x <- res) {
        if (oldDist > x) {
          isAsc = false
        } else {
          oldDist = x
        }
      }

      results(i) = isAsc
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp, 30)

      results(i) = res.size <= 30
    }
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "not contain query point itself" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)

    for (i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp, 30)
      results(i) = !res.contains(qp._1)
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }

  "Query result (if not empty)" should "be of type Arraybuffer[Int]" in {
    val f = fixture

    val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val res = f.lsh.query(qp, 30)
    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    val arr = ArrayBuffer[Int]()
    val i:Int = 0
    if(res.nonEmpty) {
      assert(res(0).getClass == i.getClass)
    }

    assert(arr.getClass == res.getClass)
  }

  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp, 30)
      results(i) = res.size == res.distinct.size
    }

    // Cleaning up
    Await.result(f.system.terminate(), timeout.duration)

    assert(results.forall(_ == true))
  }
}
