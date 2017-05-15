package lsh

import java.io.File

import actors._
import akka.actor.ActorRef
import measures.{Distance, Euclidean}
import messages.{InitRepetition, Query, Stop}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{Await, Future}
import akka.util.Timeout

import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import akka.pattern.ask
import datastructures.Table
import hashfunctions.HashFunction
import multiprobing.{PQ, ProbeScheme, TwoStep}
import tools.{CandSet, QuickSelect}

import scala.io.Source
import scala.util.Random


trait LSHStructure[Descriptor, Query, FileSet] {
  def build(fileSet:FileSet, n:Int, parserFac:DisaParserFac[Descriptor], internalReps:Int, hfFac:HashFunctionFactory[Descriptor], pgenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[Descriptor], seed:Long):Unit
  def query(qp:Query, k:Int) : CandSet
  var cands:CandSet = _
}

trait Binary {
  implicit object PQOrd extends Ordering[Int] {
    var dists:ArrayBuffer[Double] = _
    def compare(x: Int, y: Int) = dists(x).compare(dists(y))
  }

  var pq:mutable.PriorityQueue[Int] = _
  var eucDataSet:Array[(Int, Array[Float])] = _

  def knn(cands:CandSet, qp:Array[Float], k:Int) : Unit = {
    // Assuming that cands has size >= k

    PQOrd.dists = cands.dists

    var l = 0
    // fill up the queue with initial data
    while(this.pq.size < k) {
      cands.dists(l) = Euclidean.measure(this.eucDataSet(cands.ids(l))._2, qp)
      this.pq.enqueue(l)
      l+=1
    }

    while(l < cands.size) {
      cands.dists(l) = Euclidean.measure(this.eucDataSet(cands.ids(l))._2, qp)

      if(cands.dists(l) < cands.dists(pq.head)) {
        this.pq.dequeue()
        this.pq.enqueue(l)
      }
      l+=1
    }

    // resetting counter of cands to make sure we only consider k from this point
    cands.softReset
    while(pq.nonEmpty) {
      val candsIndex = pq.dequeue()
      cands.nonDistinctAdd(this.eucDataSet(cands.ids(candsIndex))._1, cands.dists(candsIndex))
    }
  }
}

trait LSHStructureSingle[Descriptor, Query, FileSet] extends LSHStructure[Descriptor, Query, FileSet] {
  var dataSet:Array[Descriptor] = _
  var lastDataSetDir:String = " "
  var repetitions:Array[Table[Descriptor]] = _
  var probeGenerator:ProbeScheme[Descriptor] = _
  var hashFunctions:Array[HashFunction[Descriptor]] = _
  var rnd:Random = _
  var futures:Array[Future[Any]] = _
  var maxCands:Int = _
  var distance:Distance[Descriptor] = _

  def clear():Unit = {
    if(this.repetitions != null) {
      for(i <- this.repetitions.indices) {
        this.repetitions(i).clear
        this.repetitions(i) = null
      }
      this.repetitions = null
    }
    if(this.hashFunctions != null) {
      for(i <- this.hashFunctions.indices) {
        this.hashFunctions(i) = null
      }
      this.hashFunctions = null
    }
    this.probeGenerator = null
    this.futures = null
    System.gc()
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
  def initRepetitions(hfFac:HashFunctionFactory[Descriptor], n:Int, functions:Int, dimensions:Int):Unit = {
    println("Initializing repetitions...")

    //var i = 0
    for (i <- this.repetitions.indices) {
      this.repetitions(i) = new Table[Descriptor]({
        this.hashFunctions(i) = hfFac(functions, this.rnd.nextLong(), dimensions)
        this.hashFunctions(i)
      }, this.dataSet)

      this.futures(i) = Future {
        buildRepetition(i, n)
      }

    }

    implicit val timeout = Timeout(20.hours)
    Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)
  }

  def buildDataSet(filePath:String, n:Int, dim:Int, parserFac: DisaParserFac[Descriptor], dataSetFac:DataSetFac[Descriptor]):Unit = {
    this.dataSet = dataSetFac(n)
    val parser = parserFac(filePath, dim)
    // Loading in dataset
    println("Loading dataset...")
    val percentile = n / 100
    var c = 0
    while (parser.hasNext) {
      if (c % percentile == 0) println(c * 100 / n)
      this.dataSet(c) = parser.next._2
      c += 1
    }

    this.lastDataSetDir = filePath
    println("done loading dataset...")
  }
}

trait LSHStructureDistributed[Descriptor, Query, FileSet] extends LSHStructure[Descriptor, Query, FileSet] {
  var nodes:Array[ActorRef] = _
  var futureResults:Array[Future[Any]] = _  // TODO Cannot use array in .sequence method, ... consider another approach.
  implicit val timeout = Timeout(20.hours)

  def getCands(qp:Descriptor, k:Int) = {

    // Reset cands set
    this.cands.reset

    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < nodes.length) {
      futureResults(i) = nodes(i) ? Query(qp, k)
      i += 1
    }

    // Wait for all results to return
    var j = 0
    while(j < futureResults.length) {
      var bucket = Await.result(futureResults(j), timeout.duration).asInstanceOf[(Array[Int], Array[Double])]

      var l = 0
      while (l < bucket._1.size) {
        this.cands+=(bucket._1(l), bucket._2(l))
        l += 1
      }
      j += 1
    }
  }

  def destroy : Unit = {
    println("initiated shut down sequence..")
    var i = 0
    while(i < nodes.length) {
      nodes(i) ! Stop
      i += 1
    }
  }
}


class LSHNumericSingle extends LSHStructureSingle[Array[Float], Array[Float], String] {

  override def query(qp: Array[Float], k:Int): CandSet = {
    /*
    // Generate probes
    this.probeGenerator.generate(qp)
    var nextBucket: (Int, Long) = null
    this.cands.reset

    // Contains pointers to the dataset

    var j, c = 0
    var index = 0

    // Collect cands
    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      var bucket = this.repetitions(nextBucket._1).query(nextBucket._2)
      j = 0
      while (j < bucket.size) {
        this.cands+=(this.dataSet(bucket(j))._1, this.distance.measure(this.dataSet(bucket(j))._2, qp))
        c += 1
        j += 1
      }
    }

    if(cands.size > k) {
      cands<=QuickSelect.selectKthDist(this.cands.dists, k-1, cands.size-1)
      cands.take(k)
      cands
    } else {
      cands
    }
*/
    ???
  }


  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {
/*    clear()

    this.rnd = new Random(seed)
    this.hashFunctions = new Array(internalReps)
    this.repetitions = new Array(internalReps)
    this.futures = new Array(internalReps)
    this.maxCands = maxCands
    this.distance = simMeasure
    this.cands = new CandSet(maxCands)

    if(fileSet != this.lastDataSetDir) {
      this.buildDataSet(fileSet, n, dimensions, parserFac)
    }

    this.initRepetitions(hfFac,n,functions,dimensions)

    // Initializing the pgenerator
    this.probeGenerator = pgenerator.toLowerCase match {
      case "pq" => new PQ(functions, this.hashFunctions)
      case "twostep" => new TwoStep(functions, this.hashFunctions)
      case _ => throw new Exception("unknown probescheme")
    }

    System.gc()*/
    ???
  }
}

class LSHNumericDistributed(repetitions:Array[ActorRef]) extends LSHStructureDistributed[Array[Float], Array[Float], String] {
  this.nodes = repetitions
  var idLookupMap:Array[Int] = _
  var lastLookupMap = " "
  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pGenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {

    this.cands = new CandSet(maxCands)
    this.futureResults = new Array(nodes.length)
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet, n, parserFac, DataSetFacNumeric, internalReps, hfFac, pGenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }
    if(this.lastLookupMap != fileSet) {
      println("building lookupMap")
      this.idLookupMap = new Array(n)
      val parser = Source.fromFile(new File(fileSet)).getLines()
      var j = 0
      while(j < n) {
        // TODO this can be done much faster (dont do split on whole line)
        this.idLookupMap(j) = parser.next().split(" ").head.toInt
        j+=1
      }
      this.lastLookupMap = fileSet
      println("done building lookupmap")
    }

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
    System.gc()
  }

  override def query(qp: Array[Float], k: Int): CandSet = {
    getCands(qp, k)

    if(this.cands.size > k) {
      cands<=QuickSelect.selectKthDist(cands.dists, k-1, cands.size)
      var i = 0
      while(i < cands.size) {
        cands.ids.update(i, this.idLookupMap(cands.ids(i)))
        i+=1
      }
      cands.take(k)
    } else {
      var j = 0
      while(j < cands.size) {
        cands.ids.update(j, this.idLookupMap(cands.ids(j)))
        j+=1
      }
    }
    this.cands
  }
}
class LSHBinaryDistributed(repetitions:Array[ActorRef]) extends Binary with LSHStructureDistributed[mutable.BitSet, (mutable.BitSet, Array[Float], Int),  (String, String)] {

  this.nodes = repetitions
  var lastEucDataSet = " "
  this.pq = new mutable.PriorityQueue[Int]

  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[mutable.BitSet], internalReps: Int, hfFac: HashFunctionFactory[mutable.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[mutable.BitSet], seed: Long): Unit = {
    this.futureResults = new Array(nodes.length)
    this.cands = new CandSet(maxCands)

    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet._1, n, parserFac, DataSetBitSet, internalReps, hfFac, pgenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    if(this.lastEucDataSet != fileSet._2) {
      println("loading internal euclidean dataset...")
      this.eucDataSet = DisaParserFacNumeric(fileSet._2,dimensions).toArray
      this.lastEucDataSet = fileSet._2
    }

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
    System.gc()
  }


  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): CandSet = {

    // Search euclidean space (this will return k results)
    // calling getCands with qp,qp._3 will return specified 'knnmax' value for knn to linear scan over
    this.getCands(qp._1, qp._3)

    // Search euclidean space (with knn size set)
    this.knn(this.cands, qp._2, {
      if (cands.size < k) cands.size
      else k
    })
    this.cands
  }
}

class LSHBinarySingle extends Binary with LSHStructureSingle[mutable.BitSet, (mutable.BitSet, Array[Float], Int), (String, String)] {
  var lastEucDataSetDir:String = " "

  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[mutable.BitSet], internalReps: Int, hfFac: HashFunctionFactory[mutable.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[mutable.BitSet], seed: Long): Unit = {
/*    this.clear()
    this.pq = null
    this.rnd = new Random(seed)
    this.pq = new mutable.PriorityQueue[Int]
    this.cands = new CandSet(maxCands)

    this.hashFunctions = new Array(internalReps)
    this.repetitions = new Array(internalReps)
    this.futures = new Array(internalReps)
    this.maxCands = maxCands
    this.distance = simMeasure

    if(fileSet._1 != this.lastDataSetDir) {
      this.buildDataSet(fileSet._1, n, dimensions, parserFac)
    }

    if(fileSet._2 != this.lastEucDataSetDir) {
      println("Loading euc dataset!")
      this.eucDataSet = DisaParserFacNumeric(fileSet._2, 128).toArray
      this.lastEucDataSetDir = fileSet._2
    }

    this.initRepetitions(hfFac,n,functions,dimensions)

    // Initializing the pgenerator
    this.probeGenerator = pgenerator.toLowerCase match {
      case "twostep" => new TwoStep(functions, this.hashFunctions)
      case _ => throw new Exception("unknown probescheme")
    }

    System.gc()*/
    ???
  }

  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): CandSet = {
/*
    // Generate probes
    this.probeGenerator.generate(qp._1)
    var nextBucket: (Int, Long) = null

    // Contains pointers to the dataset
    var candSet: ArrayBuffer[Int] = null

    var j, c = 0
    var index = 0

    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      var bucket = this.repetitions(nextBucket._1).query(nextBucket._2)
      j = 0
      while (j < bucket.size) {
        this.cands+=(bucket(j), this.distance.measure(this.dataSet(bucket(j))._2, qp._1))
        c += 1
        j += 1
      }
    }

    // Search euclidean space (with knn size set)
    if(cands.size > qp._3) {
      cands<=QuickSelect.selectKthDist(cands.dists, qp._3-1, cands.size-1)
      this.knn(cands, qp._2, k)
      this.cands
    } else {
      this.knn(cands, qp._2, {
        if(cands.size < k) cands.size
        else k
      })
      this.cands
    }*/
    ???
  }
}

