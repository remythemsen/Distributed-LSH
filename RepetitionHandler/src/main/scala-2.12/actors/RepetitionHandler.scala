package actors

import java.io.File

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import datastructures.Table
import hashfunctions.{BitHash, HashFunction, Hyperplane}
import io.Parser.{DisaParser, DisaParserBinary, DisaParserNumeric}
import measures.Distance
import messages._
import multiprobing.{PQ, ProbeScheme, TwoStep}
import tools.{SAQuickSelect, SQuickSelect, SVQuickSelect}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random



class RepetitionHandler[A] extends Actor {

  // Set of internal repetitions
  private var repetitions:Array[Table[A]] = _
  private var simMeasure:Distance[A] = _
  private var hfFac:HashFunctionFactory[A] = _

  // Internal lookup map for vectors in datastructure
  private var dataSet:Array[(Int, A)] = _
  private var dataSetVisited:Array[Boolean] = _
  private var probeGenerator:ProbeScheme[A] = _
  private var hashFunctions:Array[HashFunction[A]] = _
  private var maxCands:Int = _
  private var resultSet:Array[(Int, Double)] = _

  // Reusable array for hashed keys (query)
  private var keys:Array[(Int,Long)] = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetition(buildFromFile, n, internalReps, hashFunctionFac, probeScheme, qMaxCands, functions, dimensions, distance, seed) =>

      this.simMeasure = distance.asInstanceOf[Distance[A]]
      this.dataSet = new Array(n)
      this.hfFac = hashFunctionFac.asInstanceOf[HashFunctionFactory[A]]
      this.dataSetVisited = Array.fill[Boolean](n)(false)
      this.maxCands = qMaxCands
      this.hashFunctions = new Array(internalReps)
      this.keys = new Array(internalReps)
      val rnd = new Random(seed)

      val parser:DisaParser[A] = hashFunctionFac match {
        case
      }
      val parser:DisaParser[A] = new DisaParser[A](Iterator[String](), 128)

      // Loading in dataset
      println("Loading dataset...")
      val file = new File(buildFromFile)
      val percentile = n / 100
      var c = 0
      while (parser.hasNext) {
        if (c % percentile == 0) println(c * 100 / n)
        this.dataSet(c) = parser.next
        c += 1
      }

      // Initializing internal repetitions
      println("Initializing repetitions...")
      this.repetitions = new Array(internalReps)
      val futures: Array[Future[Any]] = new Array(internalReps)

      //var i = 0
      for (i <- 0 until internalReps) {
        this.repetitions(i) = new Table({
              this.hashFunctions(i) = this.hfFac(functions, rnd.nextLong(), dimensions)
              this.hashFunctions(i)
        })

        futures(i) = Future {
          buildRepetition(i, n)
        }
      }

      // Initializing the pgenerator
      this.probeGenerator = probeScheme.toLowerCase match {
        case "pq" => new PQ(functions, this.hashFunctions)
        case "twostep" => new TwoStep(functions, this.hashFunctions)
      }


      implicit val timeout = Timeout(20.hours)
      Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)


      sender ! true


    case Query(qp, k) => // Returns Array[(Int,Double)]
      // Generate probes
      this.probeGenerator.generate(qp.asInstanceOf[A])
      val candidates: ArrayBuffer[(Int, Double)] = new ArrayBuffer()
      //this.resultSet = new Array(k)

      var nextBucket: (Int, Long) = null

      // Contains pointers to the dataset
      var candSet: ArrayBuffer[Int] = null

      var j, c = 0
      var index = 0

      // Grab candidates from each probe bucket, take only distinct, measure dist to qp
      while (this.probeGenerator.hasNext() && c <= this.maxCands) {
        nextBucket = this.probeGenerator.next()
        candSet = this.repetitions(nextBucket._1).query(nextBucket._2)
        j = 0
        while (j < candSet.size) {
          // TODO c < maxcands are checked twice
          if (!dataSetVisited(candSet(j))) {
            index = candSet(j)
            val dist = this.simMeasure.measure(this.dataSet(index)._2, qp.asInstanceOf[A])
            if (dist > 0.0) {
              // if it's not the qp itself
              candidates += Tuple2(index, dist)
              c += 1
              dataSetVisited(index) = true
            }
          }
          j += 1
        }
      }

      // Find kth Distance
      // TODO Check correctness of k
      // TODO Find different version of quickselect
      if (candidates.nonEmpty) {
        val kthDist = SAQuickSelect.quickSelect(candidates, {
          if (candidates.length < k) candidates.size - 1
          else k
        })

        sender ! {
          // filter for distances smaller than the kth
          candidates.filter(_._2 < kthDist)

        }
      } else {
        sender ! ArrayBuffer()
      } // send empty set back

      // Cleaning up
      var h = 0
      while (h < candidates.size) {
        dataSetVisited(candidates(h)._1) = false
        h += 1
      }

  }


  def buildRepetition(mapRef:Int, dataSize:Int):Boolean = {
    var j = 0
    var percentile = dataSize / 100
    while (j < this.dataSet.length) {
      // Insert key value pair of key generated from vector, and value: Index in dataSet
      if (j % percentile == 0) println(j * 100 / dataSize)
      this.repetitions(mapRef) += (this.dataSet(j), j)
      j += 1
    }
    true
  }
}
abstract class HashFunctionFactory[A] {
  def apply(k:Int, seed:Long, numOfDim:Int):HashFunction[A]
}
case object HyperplaneFactory extends HashFunctionFactory[Array[Float]] {
  override def apply(k:Int, seed:Long, numOfDim:Int): HashFunction[Array[Float]] = {
    Hyperplane(k, seed, numOfDim)
  }
}
case object BitHashFactory extends HashFunctionFactory[mutable.BitSet] {
  override def apply(k:Int, seed:Long, numOfDim:Int): HashFunction[mutable.BitSet] = {
    BitHash(k, seed, numOfDim)
  }
}
