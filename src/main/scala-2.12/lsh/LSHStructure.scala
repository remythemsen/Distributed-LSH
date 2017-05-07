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

trait Binary {
  implicit object Ord extends Ordering[(Int, Double, Int)] {
    def compare(x: (Int, Double, Int), y: (Int, Double, Int)) = x._2.compare(y._2)
  }

  var pq:mutable.PriorityQueue[Int] = _
  var eucDataSet:Array[(Int, Array[Float])] = _

  def knn(cands:ArrayBuffer[(Int, Double, Int)], qp:Array[Float], k:Int) : ArrayBuffer[(Int, Double, Int)] = {
    var l = 0
    var dists = new Array[Double](cands.length)
    while(l < cands.size) {
      // TODO Verify that euclidean is in fact better than cosineUnit here (97% vs 95% in tests so far)
      dists(l) = Euclidean.measure(this.eucDataSet(cands(l)._3)._2, qp)

      // fill up the queue with initial data
      while(this.pq.size < k) this.pq.enqueue(l)
      // initial head distance

      if(dists(l) < dists(pq.head)) {
        this.pq.dequeue()
        this.pq.enqueue(l)
        // update headDist
      }
      l+=1
    }

    val result:ArrayBuffer[(Int, Double, Int)] = new ArrayBuffer()
    var m = 0
    while(m < k) {
      val id = pq.dequeue()
      result+=cands(id)
      m+=1
    }
    result
  }

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
    System.gc()
  }

  override def query(qp: Array[Float], k: Int): ArrayBuffer[(Int, Double, Int)] = {
    val cands = getCands(qp, k).distinct
    val kthDist = QuickSelect.selectKthDist(cands, {
      if (cands.size < k) cands.size - 1
      else k-1
    })
    cands.filter(_._2 <= kthDist)
  }
}

class LSHBinaryDistributed(repetitions:Array[ActorRef]) extends Binary with LSHStructureDistributed[mutable.BitSet, (mutable.BitSet, Array[Float], Int),  (String, String)] {
  var lastEucDataSet = " "
  this.pq = new mutable.PriorityQueue[Int]

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
    System.gc()
  }


  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): ArrayBuffer[(Int, Double, Int)] = {

    // Search euclidean space (this will return k results)
    // calling getCands with qp,qp._3 will return specified 'knnmax' value for knn to linear scan over
    this.knn(this.getCands(qp, qp._3).distinct, qp._2, k)

  }
}

class LSHBinarySingle extends Binary with LSHStructureSingle[mutable.BitSet, (mutable.BitSet, Array[Float], Int), (String, String)] {
  var lastEucDataSetDir:String = " "
  this.pq = new mutable.PriorityQueue[Int]

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

    // filter by distinct < kth(max knn'th) dist
    val kthDist = QuickSelect.selectKthDist(distinctCandidates, {
      if (distinctCandidates.size < qp._3) distinctCandidates.size - 1
      else qp._3 - 1
    })

    // Search euclidean space (with knn size set)
    this.knn(distinctCandidates.filter(_._2 <= kthDist), qp._2, k)
  }
}

