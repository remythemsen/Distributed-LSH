package lsh
import java.io.File

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

import scala.io.Source
import scala.util.Random


trait LSHStructure[Descriptor, Query, FileSet] {
  def build(fileSet:FileSet, n:Int, parserFac:DisaParserFac[Descriptor], internalReps:Int, hfFac:HashFunctionFactory[Descriptor], pgenerator:String, maxCands:Int, functions:Int, dimensions:Int, simMeasure:Distance[Descriptor], seed:Long):Unit
  def query(qp:Query, k:Int):ArrayBuffer[(Int, Double)]
}

trait Binary {
  implicit object Ord extends Ordering[(Int, Double)] {
    def compare(x: (Int, Double), y: (Int, Double)) = x._2.compare(y._2)
  }

  var pq:mutable.PriorityQueue[(Int,Double)] = _
  var eucDataSet:Array[(Int, Array[Float])] = _

  def knn(cands:ArrayBuffer[(Int, Double)], qp:Array[Float], k:Int) : ArrayBuffer[(Int, Double)] = {
    var l = 0
    while(l < cands.size) {

      // fill up the queue with initial data
      while(this.pq.size < k) {
        this.pq.enqueue((this.eucDataSet(cands(l)._1)._1,Euclidean.measure(this.eucDataSet(cands(l)._1)._2, qp)))
        l+=1
      }

      // TODO Verify that euclidean is in fact better than cosineUnit here (97% vs 95% in tests so far)
      val dist = Euclidean.measure(this.eucDataSet(cands(l)._1)._2, qp)

      if(dist < pq.head._2) {
        this.pq.dequeue()
        this.pq.enqueue((this.eucDataSet(cands(l)._1)._1, dist))
        // update headDist
      }
      l+=1
    }

    val result:ArrayBuffer[(Int, Double)] = new ArrayBuffer()
    var m = 0
    while(m < k) {
      result += pq.dequeue()
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
      this.repetitions(mapRef) += (j, this.dataSet(j)._2)
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
  var futureResults:Array[Future[Any]] = _  // TODO Cannot use array in .sequence method, ... consider another approach.
  implicit val timeout = Timeout(20.hours)
  var candidates:ArrayBuffer[(Int,Double)] = new ArrayBuffer[(Int, Double)]()

  def getCands(qp:Query, k:Int) : ArrayBuffer[(Int, Double)] = {
    //val filterMap = new mutable.HashMap[Int, Boolean]

    // for each rep, send query, wait for result from all. return set
    var i = 0
    while(i < nodes.length) {
      futureResults(i) = nodes(i) ? Query(qp, k)
      i += 1
    }

    // Wait for all results to return
    var j = 0
    while(j < futureResults.length) {
      candidates ++= Await.result(futureResults(j), timeout.duration).asInstanceOf[ArrayBuffer[(Int, Double)]]
      j+=1
    }

    candidates

/*    // Wait for all results to return
    var j, g, c = 0
    while(j < futureResults.length) {
      val candSet = Await.result(futureResults(j), timeout.duration).asInstanceOf[ArrayBuffer[(Int, Double, Int)]]
      while(g < candSet.length) {
        if(!filterMap(candSet(g)._3)) {
          filterMap(candSet(g)._3) = true
          if(c >= candidates.size) candidates += candSet(g)
          else candidates(c) = candSet(g)
          c+=1
        }
        g+=1
      }
      j+=1
    }

    // Get distinct set
    (c, candidates)*/
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

  override def query(qp: Array[Float], k:Int): ArrayBuffer[(Int, Double)] = {
    // Generate probes
    this.probeGenerator.generate(qp)
    val candidates: ArrayBuffer[(Int, Double)] = new ArrayBuffer()
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
        candidates += Tuple2(this.dataSet(index)._1, this.distance.measure(this.dataSet(index)._2, qp))
        c += 1
        j += 1
      }
    }

    val distinctCandidates = candidates.distinct

    if(distinctCandidates.size > k) {
      val kthDist = QuickSelect.selectKthDist(distinctCandidates, k-1)
      distinctCandidates.filter(_._2 <= kthDist).take(k)
    } else {
      distinctCandidates
    }
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
  var idLookupMap:Array[Int] = _
  override def build(fileSet: String, n: Int, parserFac: DisaParserFac[Array[Float]], internalReps: Int, hfFac: HashFunctionFactory[Array[Float]], pGenerator: String, maxCands: Int, functions: Int, dimensions: Int, simMeasure: Distance[Array[Float]], seed: Long): Unit = {

    this.futureResults = new Array(nodes.length)
    val statuses:ArrayBuffer[Future[Any]] = new ArrayBuffer(nodes.length)
    var i = 0
    while(i < nodes.length) {
      statuses += nodes(i) ? InitRepetition(fileSet, n, parserFac, internalReps, hfFac, pGenerator, maxCands/nodes.length, functions, dimensions, simMeasure, seed)
      i += 1
    }
    println("building lookupMap")
    this.idLookupMap = new Array(n)
    val parser = Source.fromFile(new File(fileSet)).getLines()
    var j = 0
    while(j < n) {
      // TODO this can be done much faster (dont do split on whole line)
      this.idLookupMap(j) = parser.next().split(" ").head.toInt
      j+=1
    }
    println("done building lookupmap")

    val res = Await.result(Future.sequence(statuses), timeout.duration).asInstanceOf[ArrayBuffer[Boolean]]
    println("Done building all repetitions!")
    System.gc()
  }

  override def query(qp: Array[Float], k: Int): ArrayBuffer[(Int, Double)] = {
    val cands:ArrayBuffer[(Int, Double)] = getCands(qp, k).map(x => (this.idLookupMap(x._1), x._2))

    if(cands.size > k) {
      val kthDist = QuickSelect.selectKthDist(cands, k-1)
      cands.filter(_._2 <= kthDist).take(k)
    } else {
      cands
    }
  }
}

class LSHBinaryDistributed(repetitions:Array[ActorRef]) extends Binary with LSHStructureDistributed[mutable.BitSet, (mutable.BitSet, Array[Float], Int),  (String, String)] {
  var lastEucDataSet = " "
  this.pq = new mutable.PriorityQueue[(Int, Double)]

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


  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): ArrayBuffer[(Int, Double)] = {

    // Search euclidean space (this will return k results)
    // calling getCands with qp,qp._3 will return specified 'knnmax' value for knn to linear scan over
    val cands = this.getCands(qp, qp._3).distinct
    this.knn(cands, qp._2, {
      if(cands.size < k) cands.size
      else k
    })
  }
}

class LSHBinarySingle extends Binary with LSHStructureSingle[mutable.BitSet, (mutable.BitSet, Array[Float], Int), (String, String)] {
  var lastEucDataSetDir:String = " "
  this.pq = new mutable.PriorityQueue[(Int,Double)]

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

  override def query(qp: (mutable.BitSet, Array[Float], Int), k: Int): ArrayBuffer[(Int, Double)] = {

    // Generate probes
    this.probeGenerator.generate(qp._1)
    val candidates: ArrayBuffer[(Int, Double)] = new ArrayBuffer()
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
        index = candSet(j)
        // Insert candidate with id, and distance from qp
        candidates += Tuple2(index, this.distance.measure(this.dataSet(index)._2, qp._1))
        c += 1
        j += 1
      }
    }
    val distinctCandidates = candidates.distinct

    // Search euclidean space (with knn size set)
    if(distinctCandidates.size > qp._3) {
      val kth = QuickSelect.selectKthDist(distinctCandidates, qp._3-1)
      this.knn(distinctCandidates.filter(_._2 <= kth), qp._2, k)
    } else {
      this.knn(distinctCandidates, qp._2, {
        if(distinctCandidates.size < k) distinctCandidates.size
        else k
      })
    }
  }
}

