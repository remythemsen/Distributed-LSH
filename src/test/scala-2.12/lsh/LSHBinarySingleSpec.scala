package lsh

import java.io.File

import actors._
import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import hashfunctions.BitHash
import io.Parser.{DisaParserBinary, DisaParserNumeric}
import measures.Hamming
import org.scalatest.{FlatSpec, Matchers}
import tools.CandSet

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.io.Source
import scala.util.Random

/**
  * Created by remeeh on 28-03-2017.
  */
class LSHBinarySingleSpec extends FlatSpec with Matchers {
  implicit val timeout = Timeout(10.hours)

  def fixture = {
    new {

      // Preparing tests
      val rnd = new Random(System.currentTimeMillis())
      val k = 12
      val dim = 256
      val hashFunctions = Array(BitHash(k, rnd.nextLong, dim),BitHash(k, rnd.nextLong, dim))
      val bitDataDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/0/descriptors-1-million-reduced-128-hamming-256bit.data"
      val eucDataDir = "/home/remeeh/IdeaProjects/Distributed-LSH/data/0/descriptors-1-million-reduced-128-normalized.data"
      val dataSet = DisaParserNumeric(Source.fromFile(new File(eucDataDir)).getLines(), 128).take(50).toArray
      val dataSetBit = DisaParserBinary(Source.fromFile(new File(bitDataDir)).getLines(), 256).take(50).toArray
      val lsh = new LSHBinarySingle

      lsh.build((bitDataDir, eucDataDir), 1008263, DisaParserFacBitSet, hashFunctions.length, BitHashFactory, "twostep", 5000, k, dim, new Hamming(dim), rnd.nextLong())
    }
  }


  "Query result (if not empty)" should "be of size knn or less" in {
    val f = fixture
    val results: Array[Boolean] = new Array(50)
    // Preparing tests
    for (i <- 0 until 50) {
      val index = f.rnd.nextInt(f.dataSetBit.length)

      val qp = f.dataSetBit(index)
      val qpe = f.dataSet(index)
      val qpa = (qp._2, qpe._2, 2000)
      val res = f.lsh.query(qpa, 30)

      results(i) = res.size <= 30
    }

    assert(results.forall(_ == true))
  }


  "Query result (if not empty)" should "be of type CandSet" in {
    val f = fixture

    val index = f.rnd.nextInt(f.dataSetBit.length)

    val qp = f.dataSetBit(index)
    val qpe = f.dataSet(index)
    val qpa = (qp._2, qpe._2, 2000)
    val res = f.lsh.query(qpa, 30)

    val arr = ArrayBuffer[(Int,Double)]()
    val t:CandSet = new CandSet(100)
    assert(res.getClass == t.getClass)
  }


  "Query result (if not empty)" should "only contain distinct ids" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val index = f.rnd.nextInt(f.dataSetBit.length)
      val qp = f.dataSetBit(index)
      val qpe = f.dataSet(index)
      val qpa = (qp._2, qpe._2, 2000)
      val res = f.lsh.query(qpa, 30)
      results(i) = res.size == res.distinct.size
    }
    assert(results.forall(_ == true))
  }
  "Query result (if not empty)" should "contain (known to be in structure) qp itself" in {
    val f = fixture
    val results:Array[Boolean] = new Array(50)

    for(i <- 0 until 50) {
      val index = f.rnd.nextInt(f.dataSetBit.length)
      val qp = f.dataSetBit(index)
      val qpe = f.dataSet(index)
      val qpa = (qp._2, qpe._2, 1000)
      val res = f.lsh.query(qpa, 50)
      results(i) = res.ids.contains(qp._1)
    }
    assert(results.forall(_ == true))
  }
}
