import java.io.{File, FileInputStream, ObjectInputStream}

import akka.actor.ActorSystem
import io.{Parser, ResultWriter}
import io.Parser.DisaParser
import lsh.LSHStructure
import measures.{Cosine, Distance, Euclidean}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

case class Config(dataDir:String, n:Int, dimensions:Int, queriesDir:String, repsPrNode:Int, hashFunction:String, functions:Int, probeScheme:String, queryMaxCands:Int,  measure:Distance, seed:Long, warmupIterations:Int, knn:Int, knnStructureDir:String, outDir:String)
object RecallTest extends App {

  val INVOCATION_COUNT = 10
  val OUTDIR = "data/out"

  // Remote Repetition references:
  val ips = Source.fromFile("data/ips").getLines().next.split(" ") // Ip's of tablehandlers
  val repPort = 2552
  val repSystemName = "RepetitionSystem" // table handler Actor systemname
  val systemName = "akka.tcp://"+repSystemName+"@"
  val actorPath = "/user/Repetition"
  val testCases = Source.fromFile("data/testcases").getLines().toArray // Ip's of tablehandlers

  // Initialization
  val repetitionAddresses = for {
    ip <- ips
    repetitionAddress <- {
      Array(systemName+ip+":"+repPort+actorPath)
    }
  } yield repetitionAddress

  val system = ActorSystem("RecallTestSystem")
  println("System started")

  val lsh = new LSHStructure(for {
    address <- repetitionAddresses
    repetition <- Seq(system.actorSelection(address))
  } yield repetition)
  println("Structure initialized")



  // TEST SECTION
  val rnd = new Random(System.currentTimeMillis()) // TODO Get better random seed
  var queries:Array[(Int, Array[Float])] = _
  var dataSet:Array[(Int, Array[Float])] = _
  var lastQueriesDir = ""
  var lastDataDir = ""

  val resWriter = new ResultWriter(OUTDIR,"recall-LSH", {
    val sb = new StringBuilder
    sb.append("[ dataDir ]\t")
    sb.append("[ queriesDir ]\t")
    sb.append("[ N ]\t")
    sb.append("[ dimensions ]\t")
    sb.append("[ hashfuntion ]\t")
    sb.append("[ nodes ]\t")
    sb.append("[ repetitionsPrNode ]\t")
    sb.append("[ measure ]\t")
    sb.append("[ functions ]\t")
    sb.append("[ probescheme ]\t")
    sb.append("[ knn ]\t")
    sb.append("[ queryMaxCands ]\t")
    sb.append("[ warmUpIterations ]\t")
    sb.append("[ avgRecall ]\t")
    sb.append("[ stdDevRecall ]\t")
    sb.append("[ avgTime ]\t")
    sb.append("[ stdDevTime ]\t")
    sb.toString
  })

  var tcc = 0
  while(tcc < testCases.length) {
    println("Testcase "+(tcc+1)+" out of "+testCases.length+"...")
    val tc = testCases(tcc).split(" ")
    val config = new Config (
      tc(0),        // Datadir
      tc(1).toInt,  // N
      tc(2).toInt,  // Dimensions
      tc(3),        // queriesDir
      tc(4).toInt,  // repetitions per node
      tc(5),        // hashfunction
      tc(6).toInt,  // number of functions ( k )
      tc(7),        // probescheme
      tc(8).toInt,  // max cands considered in query before returning
      tc(9).toLowerCase match {
        case "euclidean" => Euclidean
        case "cosine" => Cosine
      },
      tc(9).toLong, // random seed for hashfunction generation
      tc(10).toInt, // Warmup iterations
      tc(11).toInt, // k nearest neighbors to be tested
      tc(13),       // knnstructure dir
      tc(14)        // output dir
    )
    println("Loading knnstructure...")
    val knnStructure = loadKNNStructure(new File(config.knnStructureDir))


    println("Initializing repetitions...")
    if(lsh.build(config.dataDir, config.n, config.repsPrNode, config.hashFunction, config.probeScheme, config.queryMaxCands, config.functions, config.dimensions,config.measure, config.seed)) {
      println("LSH repetitions has been initialized..")

      println("Loading DataSet")
      // Get dataSet, keep last set if fileDir is the same
      if(!config.dataDir.equals(lastDataDir)) {
        this.dataSet = DisaParser(Source.fromFile(new File(config.dataDir)).getLines(), config.dimensions).toArray
        this.lastDataDir = config.dataDir
      }

      println("Loading Queries")
      // Get queries, keep last set if fileDir is the same
      if(!config.queriesDir.equals(lastQueriesDir)) {
        this.queries = DisaParser(Source.fromFile(new File(config.queriesDir)).getLines(), config.dimensions).toArray
        this.lastQueriesDir = config.queriesDir
      }

      println("Warmup...")
      // TODO Warmup section?
      var i = 0
      while(i < config.warmupIterations) {
        // TODO is it fine with random queries ?
        val qRes = lsh.query(this.queries(rnd.nextInt(this.queries.length))._2, config.knn)
        qRes.head
        i+=1
      }

      println("Running queries...")
      val queryTimes:ArrayBuffer[Double] = ArrayBuffer()
      val queryRecalls:ArrayBuffer[Double] = ArrayBuffer()
      var j = 0
      while(j < this.queries.length) {
        val qp:(Int, Array[Float]) = this.queries(this.rnd.nextInt(this.queries.length))
        var qRes: ArrayBuffer[Int] = ArrayBuffer()
        var invocationTimes:Array[Double] = Array(INVOCATION_COUNT)

        // Time Test, every query is made 5 times
        var l = 0
        while(l < INVOCATION_COUNT) {
          invocationTimes(l) = timer {
            qRes = lsh.query(qp._2, config.knn)
          }
        }

        queryTimes += invocationTimes.sum / INVOCATION_COUNT

        // Recall Test
        val optimalRes = knnStructure(qp._1)

        // Here the recall is the ratio of the sum of distances to qp returned by a query
        queryRecalls += {
          val optSum = optimalRes.map(_._2).sum
          val qResSum = qRes.map(x => config.measure.measure(qp._2, dataSet(x)._2)).sum
          optSum / qResSum
        }
        j += 1
      }

      // Queries has been run. Its time to write out results
      val avgTime = queryTimes.sum / this.queries.length
      val stdDevTime:Double = {
        val variance = queryTimes.map(a => math.pow(a - avgTime, 2)).sum / queryTimes.size
        Math.sqrt(variance)
      }

      val avgRecall = queryRecalls.sum / this.queries.length
      val stdDevRecall:Double = {
        val variance = queryRecalls.map(a => math.pow(a - avgRecall, 2)).sum / queryRecalls.size
        Math.sqrt(variance)
      }

      println("Writing results...")
      // Write result as line to file
      resWriter.writeResult({
        val sb = new StringBuilder
        sb.append(config.dataDir+"\t")
        sb.append(config.queriesDir+"\t")
        sb.append(config.n+"\t")
        sb.append(config.dimensions+"\t")
        sb.append(config.hashFunction+"\t")
        sb.append(this.repetitionAddresses.length+"\t")
        sb.append(config.repsPrNode+"\t")
        sb.append(config.measure.getClass.getSimpleName+"\t")
        sb.append(config.functions+"\t")
        sb.append(config.probeScheme+"\t")
        sb.append(config.knn+"\t")
        sb.append(config.queryMaxCands+"\t")
        sb.append(config.warmupIterations+"\t")
        sb.append(avgRecall+"\t")
        sb.append(stdDevRecall+"\t")
        sb.append(avgTime+"\t")
        sb.append(stdDevTime+"\t")
        sb.toString
      })


    }
  }
  system.terminate()
  println("Testing has finished")


  def loadKNNStructure(file:File): mutable.HashMap[Int, Array[(Int, Double)]] = {
    println("Loading Optimal KNN structure")
    val objReader = new ObjectInputStream(new FileInputStream(file))
    val hashMap = objReader.readObject.asInstanceOf[mutable.HashMap[Int, Array[(Int, Double)]]]
    objReader.close()
    hashMap
  }

  def timer[R](r: => R): Double = {
    val now = System.nanoTime
    r
    val time = System.nanoTime - now
    time
  }

}
