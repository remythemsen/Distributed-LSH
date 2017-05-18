package actors

import java.io.File
import java.util

import akka.actor.{Actor, ActorSystem, PoisonPill, Props}
import akka.util.Timeout
import datastructures.Table
import hashfunctions.{BitHash, HashFunction, Hyperplane}
import io.Parser.{DisaParser, DisaParserBinary, DisaParserNumeric}
import measures.Distance
import messages._
import multiprobing.{PQ, PQ2, ProbeScheme, TwoStep}
import tools.{CandSet, QuickSelect}

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
  private var dataSet:Array[A] = _
  private var probeGenerator:ProbeScheme[A] = _
  private var hashFunctions:Array[HashFunction[A]] = _
  private var maxCands:Int = _
  private var lastDataSet:String = ""
  private var parser:DisaParser[A] = _
  private var parserFact:DisaParserFac[A] = _
  private var dsf:DataSetFac[A] = _
  private var cands:CandSet = _
  private var resultSet:(Array[Int],Array[Double]) = _

  // Reusable array for hashed keys (query)
  private var keys:Array[(Int,Long)] = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetition(buildFromFile, n, parserFac, dataSetFac, internalReps, hashFunctionFac, probeScheme, qMaxCands, functions, dimensions, distance, seed) =>
      println("recieved an init message")

      this.repetitions = null
      this.simMeasure = null
      this.hfFac = null
      this.probeGenerator = null
      this.hashFunctions = null
      this.keys = null
      this.parser = null
      this.parserFact = null
      this.dsf = null
      this.cands = null
      this.resultSet = null

      this.simMeasure = distance.asInstanceOf[Distance[A]]
      this.hfFac = hashFunctionFac.asInstanceOf[HashFunctionFactory[A]]
      this.maxCands = qMaxCands
      this.hashFunctions = new Array(internalReps)
      this.keys = new Array(internalReps)
      var rnd = new Random(seed)
      this.parserFact = parserFac.asInstanceOf[DisaParserFac[A]]
      this.parser = parserFact(buildFromFile,dimensions)
      this.dsf = dataSetFac.asInstanceOf[DataSetFac[A]]
      this.cands = new CandSet(qMaxCands)

      if(buildFromFile != this.lastDataSet) {
        this.dataSet = dsf(n)
        // Loading in dataset
        println("Loading dataset...")
        val percentile = n / 100
        var c = 0
        while (parser.hasNext) {
          if (c % percentile == 0) println(c * 100 / n)
          this.dataSet(c) = parser.next._2
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
        }, this.dataSet)

        futures(i) = Future {
          buildRepetition(i, n)
        }
      }


      // Initializing the pgenerator
      this.probeGenerator = probeScheme.toLowerCase match {
        case "pq" => new PQ(functions, this.hashFunctions)
        case "pq2" => new PQ2(functions, this.hashFunctions)
        case "twostep" => new TwoStep(functions, this.hashFunctions)
        case _ => throw new Exception("unknown probescheme")
      }



      implicit val timeout = Timeout(20.hours)
      Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)
      System.gc

      sender ! true


    case Query(qp, k) => // Returns CandSet of indexes and dists' from q
      if(this.resultSet._1.length != k) this.resultSet = Tuple2(new Array(k), new Array(k))
      this.cands.reset
      // Generate probes
      this.probeGenerator.generate(qp.asInstanceOf[A])
      var nextBucket: (Int, Long) = null

      var j, c = 0
      var index = 0

      // Collect cands
      while (this.probeGenerator.hasNext() && c <= this.maxCands) {
        nextBucket = this.probeGenerator.next()
        val bucket = this.repetitions(nextBucket._1).query(nextBucket._2)
        if(bucket!=null) {
          j = 0
          while (j < bucket.size) {
            // Storing index of descriptor and dist from qp
            if (!this.cands.distinct.contains(bucket.getInt(j))) {
              this.cands.distinct.add(bucket.getInt(j))
              this.cands += (bucket.getInt(j), this.simMeasure.measure(this.dataSet(bucket.getInt(j)), qp.asInstanceOf[A]))
            }
            j += 1
          }
        }
      }

      sender ! {
        if(cands.size > k) {
          cands<=QuickSelect.selectKthDist(cands, k-1, cands.size-1)
          cands.ids.getElements(0, resultSet._1, 0, k)
          cands.dists.getElements(0, resultSet._2, 0, k)
          this.resultSet
        } else {
          cands.ids.getElements(0, resultSet._1, 0, k)
          cands.dists.getElements(0, resultSet._2, 0, k)
          this.resultSet
        }
      }


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
      this.repetitions(mapRef) += j
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
case object DisaParserFacBitSet extends DisaParserFac[util.BitSet] {
  def apply(pathToFile:String, numOfDim:Int) : DisaParserBinary = {
    DisaParserBinary(Source.fromFile(new File(pathToFile)).getLines(), numOfDim)
  }
}
abstract class DataSetFac[A] {
  def apply(size: Int) : Array[A]
}
object DataSetFacNumeric extends DataSetFac[Array[Float]] {
  override def apply(size: Int): Array[Array[Float]] = new Array(size)
}
object DataSetBitSet extends DataSetFac[util.BitSet] {
  override def apply(size: Int): Array[util.BitSet] = new Array(size)
}
abstract class HashFunctionFactory[A] {
  def apply(k:Int, seed:Long, numOfDim:Int):HashFunction[A]
}

case object HyperplaneFactory extends HashFunctionFactory[Array[Float]] {
  override def apply(k:Int, seed:Long, numOfDim:Int): HashFunction[Array[Float]] = {
    Hyperplane(k, seed, numOfDim)
  }
}
case object BitHashFactory extends HashFunctionFactory[util.BitSet] {
  override def apply(k:Int, seed:Long, numOfDim:Int): HashFunction[util.BitSet] = {
    BitHash(k, seed, numOfDim)
  }
}
