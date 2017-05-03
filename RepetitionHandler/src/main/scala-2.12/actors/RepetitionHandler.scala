package actors

import java.io.File

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import datastructures.Table
import hashfunctions.{BitHash, HashFunction, Hyperplane}
import io.Parser.{DisaParser, DisaParserBinary, DisaParserNumeric}
import measures.Distance
import messages._
import multiprobing.{PQ, ProbeScheme, TwoStep}
import tools.QuickSelect

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
  private var probeGenerator:ProbeScheme[A] = _
  private var hashFunctions:Array[HashFunction[A]] = _
  private var maxCands:Int = _
  private var lastDataSet:String = ""

  // Reusable array for hashed keys (query)
  private var keys:Array[(Int,Long)] = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetition(buildFromFile, n, parserFac, internalReps, hashFunctionFac, probeScheme, qMaxCands, functions, dimensions, distance, seed) =>
      println("recieved an init message")

      this.simMeasure = distance.asInstanceOf[Distance[A]]
      this.hfFac = hashFunctionFac.asInstanceOf[HashFunctionFactory[A]]
      this.maxCands = qMaxCands
      this.hashFunctions = new Array(internalReps)
      this.keys = new Array(internalReps)
      val rnd = new Random(seed)
      val parserFact:DisaParserFac[A] = parserFac.asInstanceOf[DisaParserFac[A]]
      val parser:DisaParser[A] = parserFact(buildFromFile,dimensions)


      if(buildFromFile != this.lastDataSet) {
        this.dataSet = new Array(n)
        // Loading in dataset
        println("Loading dataset...")
        val percentile = n / 100
        var c = 0
        while (parser.hasNext) {
          if (c % percentile == 0) println(c * 100 / n)
          this.dataSet(c) = parser.next
          c += 1
        }
        this.lastDataSet = buildFromFile
      } else {
        println("Dataset already loaded.. ")
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
        case _ => throw new Exception("unknown probescheme")
      }



      implicit val timeout = Timeout(20.hours)
      Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)


      sender ! true


    case Query(qp, k) => // Returns Array[(Int,Double)]
      // Generate probes
      this.probeGenerator.generate(qp.asInstanceOf[A])
      val candidates: ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
      //this.resultSet = new Array(k)

      var nextBucket: (Int, Long) = null

      // Contains pointers to the dataset
      var candSet: ArrayBuffer[Int] = null

      var j, c = 0
      var index = 0

      while (this.probeGenerator.hasNext() && c <= this.maxCands) {
        nextBucket = this.probeGenerator.next()
        candSet = this.repetitions(nextBucket._1).query(nextBucket._2)
        j = 0
        while (j < candSet.size) {
          // TODO c < maxcands are checked twice
          index = candSet(j)
          // Insert candidate with id, and distance from qp
          candidates += Tuple3(this.dataSet(index)._1, this.simMeasure.measure(this.dataSet(index)._2, qp.asInstanceOf[A]), index)
          c += 1
          j += 1
        }
      }

      // Find kth Distance
      // TODO Check correctness of k
      // TODO Find different version of quickselect
      if (candidates.nonEmpty) {
        val distinctCands = candidates.distinct
        val kthDist = QuickSelect.selectKthDist(distinctCands, {
          if (distinctCands.size < k) distinctCands.size - 1
          else k-1
        })
        sender ! {
          // filter for distances smaller than the kth
          distinctCands.filter(_._2 <= kthDist)
        }
      } else {
        sender ! ArrayBuffer[(Int,Double,Int)]()
      } // send empty set back


    case Stop =>
      println("Stopping...")
      context.self ! PoisonPill
      println("Terminating system...")
      context.system.terminate()
      println("System terminated")
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
abstract class DisaParserFac[A] {
  def apply(pathToFile:String, numOfDim:Int):DisaParser[A]
}
case object DisaParserFacNumeric extends DisaParserFac[Array[Float]] {
  def apply(pathToFile:String, numOfDim:Int) : DisaParserNumeric = {
    DisaParserNumeric(Source.fromFile(new File(pathToFile)).getLines(), numOfDim)
  }
}
case object DisaParserFacBitSet extends DisaParserFac[mutable.BitSet] {
  def apply(pathToFile:String, numOfDim:Int) : DisaParserBinary = {
    DisaParserBinary(Source.fromFile(new File(pathToFile)).getLines(), numOfDim)
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
