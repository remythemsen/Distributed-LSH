package lsh

import java.io.File
import java.util

import actors._
import akka.actor.ActorRef
import measures.Distance
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
import io.Parser.DisaParserNumeric
import multiprobing.{PQ, PQ2, ProbeScheme, TwoStep}
import tools.Tools.PQOrd
import tools.{CandSet, QuickSelect, Tools}

import scala.io.Source
import scala.util.Random


trait LSHStructure[Descriptor, Query, FileSet] {
  def build(fileSet:FileSet, n:Int, parserFac:DisaParserFac[Descriptor], internalReps:Int, hfFac:HashFunctionFactory[Descriptor], pgenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[Descriptor], seed:Long):Unit
  def query(qp:Query, k:Int) : CandSet
  var cands:CandSet = _
  var idLookupMap:Array[Int] = _
  var qs:QuickSelect = _
}

trait Binary {

  var pq:mutable.PriorityQueue[Int] = _
  var eucDataSet:Array[Array[Float]] = _
  var lastEucDataSet:String = " "

  def buildEucDataSet(data:String, n:Int, dimensions:Int) = {
    if(this.lastEucDataSet != data) {
      println("Building EUC DataSet...")
      this.eucDataSet = new Array(n)
      val parser = DisaParserNumeric(Source.fromFile(data).getLines(), dimensions)
      val percentile = n / 100
      var c = 0
      while (parser.hasNext) {
        if (c % percentile == 0) println(c * 100 / n)
        this.eucDataSet(c) = parser.next._2
        c += 1
      }
      this.lastEucDataSet = data
      println("Done loading EUC Dataset...")
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
    PQOrd.dists = null
    this.qs = null
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
    if(this.lastDataSetDir != filePath) {
      println("Building Dataset (& Lookupmap)...")
      this.dataSet = dataSetFac(n)
      this.idLookupMap = new Array(n)
      val parser = parserFac(filePath, dim)
      // Loading in dataset
      val percentile = n / 100
      var c = 0
      while (parser.hasNext) {
        if (c % percentile == 0) println(c * 100 / n)
        val next = parser.next
        this.dataSet(c) = next._2
        this.idLookupMap(c) = next._1
        c += 1
      }

      this.lastDataSetDir = filePath
      println("Done Building Dataset...")
    }
  }
}

trait LSHStructureDistributed[Descriptor, Query, FileSet] extends LSHStructure[Descriptor, Query, FileSet] {
  var lastLookupMap: String = " "
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

      if(bucket!=null) {
        j = 0
        while (j < bucket._1.length) {
          if(!this.cands.distinct.contains(bucket._1(j))) {
            this.cands.distinct.add(bucket._1(j))
            this.cands+=(bucket._1(j), bucket._2(j))
          }
          j += 1
        }
      }
    }
  }

  def buildLookupMap(fileSet:String, n:Int): Unit = {
    if(this.lastLookupMap != fileSet) {
      println("Building Lookup Map...")
      this.idLookupMap = new Array(n)
      val parser = Source.fromFile(new File(fileSet)).getLines()
      var j = 0
      val percentile = n / 100
      while (parser.hasNext) {
        if (j % percentile == 0) println(j * 100 / n)
        // TODO this can be done much faster (dont do split on whole line)
        this.idLookupMap(j) = parser.next().split(" ").head.toInt
        j+=1
      }
      this.lastLookupMap = fileSet
      println("Done Building Lookup Map...")
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

    // Generate probes
    this.probeGenerator.generate(qp)
    var nextBucket: (Int, Long) = null
    this.cands.reset


    // Contains pointers to the dataset

    var j, c = 0

    // Collect cands
    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      var bucket = this.repetitions(nextBucket._1).query(nextBucket._2)
      if(bucket!=null) {
        j = 0
        while (j < bucket.size) {
          if(!this.cands.distinct.contains(this.idLookupMap(bucket.getInt(j)))) {
            this.cands.distinct.add(this.idLookupMap(bucket.getInt(j)))
            this.cands+=(this.idLookupMap(bucket.getInt(j)), this.distance.measure(this.dataSet(bucket.getInt(j)), qp))
          }
          c += 1
          j += 1
        }
      }
    }

    if(cands.size > k) {
      cands<=this.qs.selectKthDist(this.cands, k-1, cands.size-1)
      cands.take(k)
      cands
    } else {
      cands
    }


  }


  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {
    clear()

    this.rnd = new Random(seed)
    this.hashFunctions = new Array(internalReps)
    this.repetitions = new Array(internalReps)
    this.futures = new Array(internalReps)
    this.maxCands = maxCands
    this.distance = simMeasure
    this.cands = null
    this.cands = new CandSet(maxCands)
    this.qs = new QuickSelect()

    this.buildDataSet(fileSet, n, dimensions, parserFac, DataSetFacNumeric)

    this.initRepetitions(hfFac, n, functions, dimensions)

    // Initializing the pgenerator
    this.probeGenerator = pgenerator.toLowerCase match {
      case "pq" => new PQ(functions, this.hashFunctions)
      case "pq2" => new PQ2(functions, this.hashFunctions)
      case "twostep" => new TwoStep(functions, this.hashFunctions)
      case _ => throw new Exception("unknown probescheme")
    }

    System.gc()

  }
}

class LSHNumericDistributed(repetitions:Array[ActorRef]) extends LSHStructureDistributed[Array[Float], Array[Float], String] {
  this.nodes = repetitions
  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pGenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {
    PQOrd.dists = null
    this.qs = null
    this.cands = null
    this.cands = new CandSet(maxCands)
    this.futureResults = new Array(nodes.length)
    this.qs = new QuickSelect()
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet, n, parserFac, DataSetFacNumeric, internalReps, hfFac, pGenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    this.buildLookupMap(fileSet, n)

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
    System.gc()
  }

  override def query(qp: Array[Float], k: Int): CandSet = {
    this.cands.reset
    getCands(qp, k)

    if(this.cands.size > k) {
      cands<=this.qs.selectKthDist(cands, k-1, cands.size-1)
      var i = 0
      while(i < cands.size) {
        // Adding the correct ids
        cands.ids.set(i, this.idLookupMap(cands.ids.getInt(i)))
        i+=1
      }
      cands.take(k)
    } else {
      var j = 0
      while(j < cands.size) {
        // Adding the correct ids
        cands.ids.set(j, this.idLookupMap(cands.ids.getInt(j)))
        j+=1
      }
    }
    this.cands
  }
}
class LSHBinaryDistributed(repetitions:Array[ActorRef]) extends Binary with LSHStructureDistributed[util.BitSet, (util.BitSet, Array[Float], Int),  (String, String)] {

  this.nodes = repetitions

  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[util.BitSet], internalReps: Int, hfFac: HashFunctionFactory[util.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[util.BitSet], seed: Long): Unit = {
    this.qs = null
    this.cands = null
    this.pq = null
    PQOrd.dists = null
    this.futureResults = null
    this.qs = new QuickSelect()
    this.futureResults = new Array(nodes.length)
    this.cands = new CandSet(maxCands)
    this.pq = new mutable.PriorityQueue[Int]()(PQOrd)

    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet._1, n, parserFac, DataSetBitSet, internalReps, hfFac, pgenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    this.buildEucDataSet(fileSet._2, n, dimensions)

    this.buildLookupMap(fileSet._1, n)

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
    System.gc()
  }

  override def query(qp: (util.BitSet, Array[Float], Int), k: Int): CandSet = {
    this.cands.reset
    // Search euclidean space (this will return k results)
    // calling getCands with qp,qp._3 will return specified 'knnmax' value for knn to linear scan over
    this.getCands(qp._1, qp._3)

    // Search euclidean space (with knn size set)
    Tools.knn(this.cands, this.eucDataSet, this.idLookupMap, this.pq, qp._2, {
      if (cands.size < k) cands.size
      else k
    })
    this.cands
  }
}

class LSHBinarySingle extends Binary with LSHStructureSingle[util.BitSet, (util.BitSet, Array[Float], Int), (String, String)] {
  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[util.BitSet], internalReps: Int, hfFac: HashFunctionFactory[util.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[util.BitSet], seed: Long): Unit = {
    this.clear()
    this.pq = null
    this.cands = null
    PQOrd.dists = null
    this.qs = new QuickSelect()
    this.rnd = new Random(seed)
    this.pq = new mutable.PriorityQueue[Int]()(PQOrd)
    this.cands = new CandSet(maxCands)

    this.hashFunctions = new Array(internalReps)
    this.repetitions = new Array(internalReps)
    this.futures = new Array(internalReps)
    this.maxCands = maxCands
    this.distance = simMeasure

    this.buildDataSet(fileSet._1, n, dimensions, parserFac, DataSetBitSet)
    this.buildEucDataSet(fileSet._2, n, dimensions)
    this.initRepetitions(hfFac,n,functions,dimensions)

    // Initializing the pgenerator
    this.probeGenerator = pgenerator.toLowerCase match {
      case "twostep" => new TwoStep(functions, this.hashFunctions)
      case _ => throw new Exception("unknown probescheme")
    }

    System.gc()
  }

  override def query(qp: (util.BitSet, Array[Float], Int), k: Int): CandSet = {

    this.cands.reset
    // Generate probes
    this.probeGenerator.generate(qp._1)
    var nextBucket: (Int, Long) = null

    // Contains pointers to the dataset

    var j, c = 0
    var index = 0

    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      var bucket = this.repetitions(nextBucket._1).query(nextBucket._2)
      if(bucket!=null) {
        j = 0
        while (j < bucket.size) {
          if(!this.cands.distinct.contains(bucket.getInt(j))) {
            this.cands.distinct.add(bucket.getInt(j))
            this.cands+=(bucket.getInt(j), this.distance.measure(this.dataSet(bucket.getInt(j)), qp._1))
          }
          c += 1
          j += 1
        }
      }
    }

    // Search euclidean space (with knn size set)
    if(cands.size > qp._3) {
      // size was bigger than the knnMax specified, we need to prune down
      cands <= this.qs.selectKthDist(cands, qp._3 - 1, cands.size - 1)
    }

    // start knn linear search
    Tools.knn(cands, this.eucDataSet, this.idLookupMap, this.pq, qp._2, {
      if(cands.size < k) cands.size
      else k
    })

    this.cands
  }
}

