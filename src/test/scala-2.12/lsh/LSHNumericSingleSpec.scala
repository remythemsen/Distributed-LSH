package lsh

import java.io.File

import actors.{DisaParserFacNumeric, HyperplaneFactory, RepetitionHandler}
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.Hyperplane
import io.Parser.DisaParserNumeric
import measures.EuclideanFast
import org.scalatest.{FlatSpec, Matchers}
import tools.CandSet

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHNumericSingleSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {

      // Preparing tests
      val rnd = new Random(System.currentTimeMillis())
      val k = 4
      val hashFunctions = Array(Hyperplane(k, rnd.nextLong, 128))
      val eucDataDir = "data/descriptors-40000-reduced-128.data"
      val dataSet = DisaParserNumeric(Source.fromFile(new File(eucDataDir)).getLines(), 128).toArray
      val lsh = new LSHNumericSingle
      val dim = 128

      lsh.build(eucDataDir, 39290, DisaParserFacNumeric, 2, HyperplaneFactory, "twostep", 49000, k, dim, EuclideanFast, rnd.nextLong())
    }
  }


  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp._2,30)

      results(i) = res.size <= 30
    }
    // Cleaning up

    assert(results.forall(_ == true))
  }


  "Query result (if not empty)" should "be of type Arraybuffer[(Int,double)]" in {
    val f = fixture

    val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
    val res = f.lsh.query(qp._2,30)

    val arr = ArrayBuffer[(Int,Double)]()
    val t:CandSet = new CandSet(100)
    assert(res.getClass == t.getClass)

  }

  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val qp = f.dataSet(f.rnd.nextInt(f.dataSet.length))
      val res = f.lsh.query(qp._2, 30)
      results(i) = res.size == res.distinct.size
    }

    assert(results.forall(_ == true))
  }
}
