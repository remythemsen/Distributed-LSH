package lsh

import actors.{DisaParserFac, DisaParserFacNumeric, HashFunctionFactory}
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
import hashfunctions.{HashFunction, Hyperplane}
import multiprobing.{PQ, ProbeScheme, TwoStep}
import tools.QuickSelect

import scala.util.Random


trait LSHStructure[Descriptor, Query, FileSet] {
  def build(fileSet:FileSet, n:Int, parserFac:DisaParserFac[Descriptor], internalReps:Int, hfFac:HashFunctionFactory[Descriptor], pgenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[Descriptor], seed:Long):Unit
  def query(qp:Query, k:Int):ArrayBuffer[(Int, Double, Int)]
}

trait LSHStructureSingle[Descriptor, Query, FileSet] extends LSHStructure[Descriptor, Query, FileSet] {
  var dataSet:Array[(Int, Descriptor)] = _
  var lastDataSetDir:String = " "
  var repetitions:Array[Table[Descriptor]] = _
  var probeGenerator:ProbeScheme[Descriptor] = _
  var hashFunctions:Array[HashFunction[Descriptor]] = _
  var rnd:Random = _
  var futures:Array[Future[Any]] = _
  var maxCands:Int = _
  var distance:Distance[Descriptor] = _


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
  def initRepetitions(hfFac:HashFunctionFactory[Descriptor], n:Int, functions:Int, dimensions:Int):Unit = {
    println("Initializing repetitions...")

    //var i = 0
    for (i <- this.repetitions.indices) {
      this.repetitions(i) = new Table({
        this.hashFunctions(i) = hfFac(functions, this.rnd.nextLong(), dimensions)
        this.hashFunctions(i)
      })

      this.futures(i) = Future {
        buildRepetition(i, n)
      }

    }

    implicit val timeout = Timeout(20.hours)
    Await.result(Future.sequence(futures.toIndexedSeq), timeout.duration)
  }

  def buildDataSet(filePath:String, n:Int, dim:Int, parserFac: DisaParserFac[Descriptor]):Unit = {
    this.dataSet = new Array(n)
    val parser = parserFac(filePath, dim)
    // Loading in dataset
    println("Loading dataset...")
    val percentile = n / 100
    var c = 0
    while (parser.hasNext) {
      if (c % percentile == 0) println(c * 100 / n)
      this.dataSet(c) = parser.next
      c += 1
    }

    this.lastDataSetDir = filePath
    println("done loading dataset...")
  }
}

trait LSHStructureDistributed[Descriptor, Query, FileSet] extends LSHStructure[Descriptor, Query, FileSet] {
  var nodes:Array[ActorRef] = _
  var futureResults:Array[Future[Any]] = _  // TODO Cannot use array in .sequence method, ... consider another method.
  implicit val timeout = Timeout(20.hours)

  def getCands(qp:Query, k:Int) : ArrayBuffer[(Int,Double,Int)] = {
    val candidates = new ArrayBuffer[(Int,Double,Int)]()

    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < nodes.length) {
      futureResults(i) = nodes(i) ? Query(qp, k)
      i += 1
    }

    // Wait for all results to return
    var j = 0
    while(j < futureResults.length) {
      candidates ++= Await.result(futureResults(j), timeout.duration).asInstanceOf[ArrayBuffer[(Int, Double, Int)]]
      j+=1
    }

    candidates
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

  override def query(qp: Array[Float], k:Int): ArrayBuffer[(Int, Double, Int)] = {
    // Generate probes
    this.probeGenerator.generate(qp)
    val candidates: ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
    var nextBucket: (Int, Long) = null

    // Contains pointers to the dataset
    var candSet: ArrayBuffer[Int] = null

    var j, c = 0
    var index = 0

    // Collect cands
    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      candSet = this.repetitions(nextBucket._1).query(nextBucket._2)
      j = 0
      while (j < candSet.size) {
        // TODO c < maxcands are checked twice
        index = candSet(j)
        // Insert candidate with id, and distance from qp
        candidates += Tuple3(this.dataSet(index)._1, this.distance.measure(this.dataSet(index)._2, qp), index)
        c += 1
        j += 1
      }
    }

    val distinctCandidates = candidates.distinct
    val kthDist = QuickSelect.selectKthDist(distinctCandidates, {
      if (distinctCandidates.size < k) distinctCandidates.size - 1
      else k
    })

    distinctCandidates.filter(_._2 < kthDist).take(k)
  }

  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {
    this.rnd = new Random(seed)
    this.hashFunctions = new Array(internalReps)
    this.repetitions = new Array(internalReps)
    this.futures = new Array(internalReps)
    this.maxCands = maxCands
    this.distance = simMeasure

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

    System.gc()
  }
}

class LSHNumericDistributed(repetitions:Array[ActorRef]) extends LSHStructureDistributed[Array[Float], Array[Float], String] {
  this.nodes = repetitions
  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pGenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {

    this.futureResults = new Array(nodes.length)
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet, n, parserFac, internalReps, hfFac, pGenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
  }

  override def query(qp: Array[Float], k: Int): ArrayBuffer[(Int, Double, Int)] = {
    val cands = getCands(qp, k).distinct
    val kthDist = QuickSelect.selectKthDist(cands, {
      if (cands.size < k) cands.size - 1
      else k-1
    })
    val res = cands.filter(_._2 <= kthDist)
    res
  }
}

class LSHBinaryDistributed(repetitions:Array[ActorRef]) extends LSHStructureDistributed[mutable.BitSet, (mutable.BitSet, Array[Float], Int),  (String, String)] {
  var eucDataSet:Array[(Int, Array[Float])] = _
  var lastEucDataSet = " "

  implicit object Ord extends Ordering[(Int, Double, Int)] {
    def compare(x: (Int, Double, Int), y: (Int, Double, Int)) = x._2.compare(y._2)
  }
  var pq:mutable.PriorityQueue[(Int, Double, Int)] = new mutable.PriorityQueue[(Int, Double, Int)]()

  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[mutable.BitSet], internalReps: Int, hfFac: HashFunctionFactory[mutable.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[mutable.BitSet], seed: Long): Unit = {
    this.futureResults = new Array(nodes.length)

    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet._1, n, parserFac, internalReps, hfFac, pgenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }

    if(this.lastEucDataSet != fileSet._2) {
      println("loading internal euclidean dataset")
      this.eucDataSet = DisaParserFacNumeric(fileSet._2,dimensions).toArray
      this.lastEucDataSet = fileSet._2
    }

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
  }

  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): ArrayBuffer[(Int, Double, Int)] = {
    val distinctCandidates = this.getCands(qp, k).distinct

    var l = 0
    while(l < distinctCandidates.size) {
      // TODO Verify that euclidean is in fact better than cosineUnit here (97% vs 95% in tests so far)
      // distance of candidate l from qp in euclidean space
      val dist = Euclidean.measure(this.eucDataSet(distinctCandidates(l)._3)._2, qp._2)
      if(pq.size < k) {
        this.pq.enqueue((distinctCandidates(l)._1, dist, distinctCandidates(l)._3))
      } else if (pq.head._2 > dist) {
        pq.dequeue()
        pq.enqueue((distinctCandidates(l)._1, dist, distinctCandidates(l)._3))
      }
      l+=1
    }
    val result:ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
    var m = 0
    while(m < k) {
      result+=pq.dequeue()
      m+=1
    }

    result
  }
}

class LSHBinarySingle extends LSHStructureSingle[mutable.BitSet, (mutable.BitSet, Array[Float], Int), (String, String)] {
  var eucDataSet:Array[(Int,Array[Float])] = _
  var lastEucDataSetDir:String = " "

  implicit object Ord extends Ordering[(Int, Double, Int)] {
    def compare(x: (Int, Double, Int), y: (Int, Double, Int)) = x._2.compare(y._2)
  }
  var pq:mutable.PriorityQueue[(Int, Double, Int)] = new mutable.PriorityQueue[(Int, Double, Int)]()

  override def build(fileSet: (String, String), n: Int, parserFac: DisaParserFac[mutable.BitSet], internalReps: Int, hfFac: HashFunctionFactory[mutable.BitSet], pgenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[mutable.BitSet], seed: Long): Unit = {
    this.rnd = new Random(seed)
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

    System.gc()
  }

  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): ArrayBuffer[(Int, Double, Int)] = {

    // Generate probes
    this.probeGenerator.generate(qp._1)
    val candidates: ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
    var nextBucket: (Int, Long) = null

    // Contains pointers to the dataset
    var candSet: ArrayBuffer[Int] = null

    var j, c = 0
    var index = 0

    // Collect cands
    while (this.probeGenerator.hasNext() && c <= this.maxCands) {
      nextBucket = this.probeGenerator.next()
      candSet = this.repetitions(nextBucket._1).query(nextBucket._2)
      j = 0
      while (j < candSet.size) {
        // TODO c < maxcands are checked twice
        index = candSet(j)
        // Insert candidate with id, and distance from qp
        candidates += Tuple3(this.dataSet(index)._1, this.distance.measure(this.dataSet(index)._2, qp._1), index)
        c += 1
        j += 1
      }
    }
    val distinctCandidates = candidates.distinct

    // If the its bithash we need to do some extra work comparing points in euclidean space
    // filter by distinct < kth(max knn'th) dist
    val kthDist = QuickSelect.selectKthDist(distinctCandidates, {
      if (distinctCandidates.size < qp._3) distinctCandidates.size - 1
      else qp._3 - 1
    })

    val knnRes = distinctCandidates.filter(_._2 < kthDist)

    // Find actual nearest neighbors from candidates set in euclidean space
    var l = 0
    while(l < knnRes.size) {
      // TODO Verify that euclidean is in fact better than cosineUnit here (97% vs 95% in tests so far)
      // distance of candidate l from qp in euclidean space
      val dist = Euclidean.measure(this.eucDataSet(knnRes(l)._3)._2, qp._2)
      if(pq.size < k) {
        this.pq.enqueue((knnRes(l)._1, dist, knnRes(l)._3))
      } else if (pq.head._2 > dist) {
        pq.dequeue()
        pq.enqueue((knnRes(l)._1, dist, knnRes(l)._3))
      }
      l+=1
    }
    val result:ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
    var m = 0
    while(m < k && pq.nonEmpty) {
      result+=pq.dequeue()
      m+=1
    }
    this.pq.clear
    result.sortBy(x => x._2)
  }
}

