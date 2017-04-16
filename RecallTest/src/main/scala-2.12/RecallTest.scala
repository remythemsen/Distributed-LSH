import java.io.File
import java.util.concurrent.TimeUnit

import akka.actor.ActorSystem
import io.Parser.DisaParser
import io.ResultWriter
import lsh.LSHStructure
import measures.{Cosine, CosineUnit, Distance, Euclidean}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

case class Config(dataDir:String, n:Int, queriesDir:String, dimensions:Int, repsPrNode:Int, hashFunction:String, functions:Int, probeScheme:String, queryMaxCands:Int, measure:Distance, seed:Long, warmupIterations:Int, knn:Int, knnSetsDir:String, outDir:String)
object RecallTest extends App {

  val INVOCATION_COUNT = 10

  // Remote Repetition references:
  val ips = Source.fromFile("data/ips").getLines().next.split(" ") // Ip's of tablehandlers
  val repPort = 2552
  val repSystemName = "RepetitionSystem" // table handler Actor systemname
  val systemName = "akka.tcp://"+repSystemName+"@"
  val actorPath = "/user/Repetition"
  val testCases = Source.fromFile("data/testcases").getLines().toArray
  var rnd:Random = _

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
  var dataSet:Array[(Int, Array[Float])] = _
  var queries:Array[(Int, Array[Float])] = _
  var lastDataDir = ""
  var lastQueriesDir = ""

  val resWriter = new ResultWriter("data/out","recall-LSH", {
    val sb = new StringBuilder
    sb.append("N ")
    sb.append("dim ")
    sb.append("hf ")
    sb.append("#nodes ")
    sb.append("#totalReps ")
    sb.append("measure ")
    sb.append("#functions ")
    sb.append("probingScheme ")
    sb.append("#knn ")
    sb.append("#queryMaxCands ")
    sb.append("warmUpIts ")
    sb.append("avgRecall ")
    sb.append("stdDevRecall ")
    sb.append("avgTime ")
    sb.append("stdDevTime ")
    sb.toString
  })

  var tcc = 0
  while(tcc < testCases.length) {
    println("Testcase "+(tcc+1)+" out of "+testCases.length+"...")
    val tc = testCases(tcc).split(" ")
    val config = new Config (
      tc(0),        // Datadir
      tc(1).toInt,  // N
      tc(2),        // queriesdir
      tc(3).toInt,  // Dimensions
      tc(4).toInt,  // repetitions per node
      tc(5),        // hashfunction
      tc(6).toInt,  // number of functions ( k )
      tc(7),        // probescheme
      tc(8).toInt,  // max cands considered in query before returning
      tc(9).toLowerCase match {
        case "euclidean" => Euclidean
        case "cosine" => Cosine
        case "cosineunit" => CosineUnit
      },
      tc(10).toLong, // random seed for hashfunction generation
      tc(11).toInt, // Warmup iterations
      tc(12).toInt, // k nearest neighbors to be tested
      tc(13),       // knnstructure dir
      tc(14)        // output dir
    )

    println("Loading knnsets...")
    val knnStructure = loadKNNSets(new File(config.knnSetsDir))


    this.rnd = new Random(config.seed)

    println("Initializing repetitions...")
    if(lsh.build(config.dataDir, config.n, config.repsPrNode, config.hashFunction, config.probeScheme, config.queryMaxCands, config.functions, config.dimensions,config.measure, rnd.nextLong)) {
      println("LSH repetitions has been initialized..")

      // Get dataSet, keep last set if fileDir is the same
      if(!config.dataDir.equals(lastDataDir)) {
        println("Dataset has not been loaded. Loading Dataset...")
        this.dataSet = DisaParser(Source.fromFile(new File(config.dataDir)).getLines(), config.dimensions).toArray
        this.lastDataDir = config.dataDir
      }

      // Get queries, keep last set if fileDir is the same
      if(!config.dataDir.equals(lastQueriesDir)) {
        println("queries has not been loaded. Loading queries...")
        this.queries = DisaParser(Source.fromFile(new File(config.queriesDir)).getLines(), config.dimensions).toArray
        this.lastQueriesDir = config.queriesDir
      }

      println("Warmup...")
      var i = 0
      while(i < config.warmupIterations) {
        val qRes = lsh.query(this.queries(rnd.nextInt(this.queries.length)), config.knn)
        if(qRes.nonEmpty) {
          qRes.head
        }
        i+=1
      }

      println("Running queries...")
      val queryTimes:ArrayBuffer[Double] = ArrayBuffer()
      val queryRecalls:ArrayBuffer[Double] = ArrayBuffer()
      var j = 0
      while(j < this.queries.length) {
        val qp:(Int, Array[Float]) = this.queries(rnd.nextInt(this.queries.length))
        var qRes: ArrayBuffer[Int] = ArrayBuffer()
        var invocationTimes:Array[Double] = new Array(INVOCATION_COUNT)

        // Time Test, every query is made 5 times
        var l = 0
        while(l < INVOCATION_COUNT) {
          invocationTimes(l) = timer {
            qRes = lsh.query(qp, config.knn)
          }
          l+=1
        }

        queryTimes += invocationTimes.sum / INVOCATION_COUNT

        // Recall Test
        val optimalRes = knnStructure(qp._1).take(config.knn)

        // Here the recall is the ratio of the sum of distances to qp returned by a query
        queryRecalls += {
          val optSum:Double = optimalRes.map(_._2).sum
          var qResSum = qRes.map(x => config.measure.measure(dataSet(x)._2, qp._2)).sum
          if(qRes.size < config.knn) {
            // Punishment
            println("optimal sum: " + optSum)
            println("punished " + qResSum +" + "+ (config.knn - qRes.size))
            qResSum += 2* (config.knn - qRes.size)
          }
          if(optSum > qResSum){
            println("WAS BIGGER :( by " + (optSum - qResSum))
            println("Qres: distances.... ")
            for(y <- qRes) {
              print(config.measure.measure(dataSet(y)._2, qp._2) + " ")

            }
          }

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
        sb.append(config.n+" ")
        sb.append(config.dimensions+" ")
        sb.append(config.hashFunction+" ")
        sb.append(this.repetitionAddresses.length+" ")
        sb.append(config.repsPrNode+" ")
        sb.append(config.measure.getClass.getSimpleName+" ")
        sb.append(config.functions+" ")
        sb.append(config.probeScheme+" ")
        sb.append(config.knn+" ")
        sb.append(config.queryMaxCands+" ")
        sb.append(config.warmupIterations+" ")
        sb.append(avgRecall+" ")
        sb.append(stdDevRecall+" ")
        sb.append((avgTime / 1E6)+"ms ")
        sb.append((stdDevTime / 1E6)+"ms")
        sb.toString
      })

    }
    tcc += 1
  }
  system.terminate()
  println("Testing has finished")


  def loadKNNSets(file:File): mutable.HashMap[Int, Array[(Int, Double)]] = {
    val map = new mutable.HashMap[Int, Array[(Int, Double)]]
    val fileLines = Source.fromFile(file).getLines()
    // format is: key,id dist,id dist, ....  for each line
    while(fileLines.hasNext) {
      val pair = fileLines.next.split(",")
      val key:String = pair.head
      val nearestNeighbors:Array[(Int, Double)] = pair.tail.map(x => {
        val set = x.split(" ")
        (set(0).toInt, set(1).toDouble)
      })
      map.put(key.toInt, nearestNeighbors)
    }
    map
  }

  def timer[R](r: => R): Double = {
    val now = System.nanoTime
    r
    val time = System.nanoTime - now
    time
  }

}
