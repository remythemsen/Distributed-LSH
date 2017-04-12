package actors

import java.io.File

import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout
import datastructures.{ProbeTableLong, ProbeTableLongMapOld}
import hashfunctions.{Crosspolytope, HashFunctionLong, Hyperplane, HyperplaneLong}
import io.Parser.DisaParser
import measures.Distance
import messages._
import multiprobing.{PQProbeGenerator, ProbeKeyGenerator, TwoStepProbeGenerator}
import tools.{SQuickSelect, SVQuickSelect}

import scala.concurrent.duration._
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random



class RepetitionHandlerProbe extends Actor {

  // Set of internal repetitions
  private var repetitions:Array[ProbeTableLong] = _
  private var simMeasure:Distance = _

  // Internal lookup map for vectors in datastructure
  private var dataSet:Array[(Int, Array[Float])] = _
  private var dataSetVisited:Array[Boolean] = _

  private var probeGenerator:ProbeKeyGenerator = _

  private var hashFunctions:Array[HashFunctionLong] = _

  private var maxCands:Int = _
  private var resultSet:Array[(Int, Double)] = _

  // Reusable array for hashed keys (query)
  private var keys:Array[(Int,Long)] = _

  override def receive: Receive = {
    // Setting or resetting a repetition
    case InitRepetitionProbe(buildFromFile, n, internalReps, hashFunction, probeScheme, qMaxCands, functions, dimensions, distance, seed) => {

      this.simMeasure = distance
      this.dataSet = new Array(n)
      this.dataSetVisited = Array.fill[Boolean](n)(false)
      this.maxCands = qMaxCands
      this.hashFunctions = new Array(internalReps)
      this.keys = new Array(internalReps)
      this.resultSet = new Array(maxCands)
      val rnd = new Random(seed)

      // Loading in dataset
      println("Loading dataset...")
      val parser = DisaParser(Source.fromFile(new File(buildFromFile)).getLines(), dimensions)
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
        this.repetitions(i) = new ProbeTableLong({
          hashFunction.toLowerCase() match {
            case "hyperplane" => {
              this.hashFunctions(i) = new HyperplaneLong(functions, rnd.nextLong(), dimensions)
              this.hashFunctions(i)
            }
            //case "crosspolytope" => new Crosspolytope(functions, rnd.nextLong(), dimensions)
          }
        })

        futures(i) = Future {
          buildRepetition(i, n)
        }
      }

      // Initializing the pgenerator
      this.probeGenerator = probeScheme.toLowerCase match {
        case "pq" => new PQProbeGenerator(functions, this.hashFunctions)
        case "twostep" => new TwoStepProbeGenerator(functions, this.hashFunctions)
      }

      implicit val timeout = Timeout(20.hours)
      Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)


      sender ! true
    }

    case Query(qp, k) => { // Returns Array[(Int,Double)]


      // Generate probes
      this.probeGenerator.generate(qp)

      var nextBucket:(Int, Long) = null
      var candSet:ArrayBuffer[Int] = null
      var j,c = 0
      var index = 0

      // Grab candidates from each probe bucket, take only distinct, measure dist to qp
      while(this.probeGenerator.hasNext() && c <= this.maxCands) {
        nextBucket = this.probeGenerator.next
        candSet = this.repetitions(nextBucket._1).query(nextBucket._2)
        j = 0
        while(j < candSet.length && c < resultSet.length ) { // TODO c < maxcands are checked twice
          if(!dataSetVisited(j)) {
            index = candSet(j)
            resultSet(c) = (index, this.simMeasure.measure(this.dataSet(index)._2, qp))
            c += 1
            dataSetVisited(index) = true
          }
          j += 1
        }
      }

      // Find kth Distance
      // TODO Check correctness of k-1
      // TODO Find different version of quickselect
      val kthDist = SQuickSelect.quickSelect(resultSet, {
        if (resultSet.length < k) resultSet.length - 1
        else k - 1
      })

      // TODO Dont use built in filter
      sender ! resultSet.filter(x => x._2 <= kthDist)

      // Cleaning up
      var h = 0
      while(h < resultSet.length) {
        dataSetVisited(resultSet(h)._1) = false
        h+=1
      }
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
