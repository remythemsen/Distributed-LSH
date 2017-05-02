import java.io.File

import actors.{BitHashFactory, DisaParserFacBitSet, DisaParserFacNumeric, HyperplaneFactory}
import io.Parser.{DisaParser, DisaParserBinary, DisaParserNumeric}
import io.ResultWriter
import scopt.OptionParser
import lsh.LSHStructure
import measures._

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Random

case class TestCase(queriesDir:String, eucQueriesDir:String, repsPrNode:Int, functions:Int, probeScheme:String, queryMaxCands:Int, measure:String, knn:Int, knnSetsPath:String)
case class Config(
                   data:String = " ",
                   dataeuc:String = " ",
                   dataSize:Int = 0,
                   dimensions:Int = 128,
                   dataType:String = "numeric",
                   nodes:File = new File("."),
                   testCases:File = new File("."),
                   invocationCount:Int = 0,
                   warmUpIterations:Int = 0,
                   outDir:String = "",
                   seed:Long = 1239801l
                 )


object RecallTest extends App {

  var bitQueries:Array[(Int, mutable.BitSet)] = _
  var queries:Array[(Int, Array[Float])] = _
  var lastQueriesDir = ""
  var eucqueries:Array[(Int, Array[Float])] = _
  var lastEucQueriesDir = ""


  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      var rnd:Random = new Random(config.seed)
      val INVOCATION_COUNT = config.invocationCount

      // Remote Repetition references:
      val nodesAddresses = Source.fromFile(config.nodes).getLines.map(x => {
        val y = x.split(":")
        "akka.tcp://RepetitionSystem@"+y(0)+":"+y(1)+"/user/Repetition"
      }).toArray

      val testCases = Source.fromFile(config.testCases).getLines().toArray

      val resWriter = new ResultWriter(config.outDir,"recall-LSH", {
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

      // What datatype ?
      config.dataType.toLowerCase() match {
        case "numeric" => {
          println("Running test as 'Numeric'")
          // Initialization
          val lsh = new LSHStructure[Array[Float]](nodesAddresses)

          // TEST SECTION

          var tcc = 0
          while(tcc < testCases.length) {
            println("Testcase "+(tcc+1)+" out of "+testCases.length+"...")
            val tc = testCases(tcc).split(" ")
            val testCase = TestCase (
              tc(0),        // queriesdir
              tc(1),        // eucqueriesDir (only used when bithash)
              tc(2).toInt,  // repetitions per node
              tc(3).toInt,  // number of functions ( k )
              tc(4),        // probescheme
              tc(5).toInt,  // max cands considered in query before returning
              tc(6),        // similarity measure
              tc(7).toInt, // k nearest neighbors to be tested
              tc(8)        // knnSetsDir dir
            )

            println("Loading knnsets...")
            val knnStructure = loadKNNSets(new File(testCase.knnSetsPath))

            println("Initializing repetitions...")
            val simMeasure = testCase.measure.toLowerCase match {
              case "euclidean" => Euclidean
              case "cosine" => Cosine
              case "cosineunit" => CosineUnit
            }

            if(lsh.build(config.data, config.dataeuc, config.dataType, config.dataSize, DisaParserFacNumeric, testCase.repsPrNode, HyperplaneFactory, testCase.probeScheme, testCase.queryMaxCands, testCase.functions, config.dimensions, simMeasure, rnd.nextLong)) {
              println("LSH repetitions has been initialized..")

              // Get queries, keep last set if fileDir is the same
              if(!testCase.queriesDir.equals(lastQueriesDir)) {
                println("queries has not been loaded. Loading queries...")
                this.queries = DisaParserNumeric(Source.fromFile(new File(testCase.queriesDir)).getLines(), config.dimensions).toArray
                this.lastQueriesDir = testCase.queriesDir
              }

              // Get euclidean queries, keep last set if fileDir is the same // Only used for bit hashing
              if(!testCase.eucQueriesDir.equals(lastEucQueriesDir)) {
                println("euc queries has not been loaded. Loading queries...")
                this.eucqueries = DisaParserNumeric(Source.fromFile(new File(testCase.eucQueriesDir)).getLines(), config.dimensions).toArray
                this.lastEucQueriesDir = testCase.eucQueriesDir
              }

              println("Warmup...")
              var i = 0
              while(i < config.warmUpIterations) {
                val index = rnd.nextInt(this.queries.length)
                val qRes = lsh.query(this.queries(index), this.eucqueries(index), testCase.knn)
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
                val index = rnd.nextInt(this.queries.length)
                val qp:(Int, Array[Float]) = this.queries(index)
                val qpeuc:(Int, Array[Float]) = this.queries(index)
                var qRes: ArrayBuffer[(Int,Double,Int)] = ArrayBuffer()
                var invocationTimes:Array[Double] = new Array(INVOCATION_COUNT)

                // Time Test, every query is made 5 times
                var l = 0
                while(l < INVOCATION_COUNT) {
                  invocationTimes(l) = timer {
                    qRes = lsh.query(qp,qpeuc, testCase.knn)
                  }
                  l+=1
                }

                queryTimes += invocationTimes.sum / INVOCATION_COUNT

                // Recall Test
                val optimalRes = knnStructure(qp._1).take(testCase.knn)
/*                println(" Opt: ")
                for(i <- optimalRes.take(10)) {
                  print("("+i._1+","+i._2+") ")
                }
                println("")
                println(" Res: ")
                for(i <- qRes.take(10)) {
                  print("("+i._1+","+i._2+") ")
                }
                println("")
                println(optimalRes.map(x => x._2).sum + " vs: " + qRes.map(x => x._2).sum)
                println("")*/

                // Here the recall is the ratio of the sum of distances to qp returned by a query
                queryRecalls += {
                  val optSum:Double = optimalRes.map(_._2).sum
                  var qResSum = qRes.map(x => x._2).sum
                  if(qRes.size < testCase.knn) {
                    // Punishment
                    println("optimal sum: " + optSum)
                    println("punished " + qResSum +" + 2*"+ (testCase.knn - qRes.size))
                    qResSum += 10*(optSum)*(testCase.knn - qRes.size)
                  }
                  if(optSum > qResSum){
                    println(" ")
                    println("opt sum was lt app sum by " + (optSum - qResSum))
                    println("for qp: "+ qp._1)
                    println("opt result ("+optimalRes.length+") : ")
                    for(y <- optimalRes) {
                      print("("+y._1+","+y._2 + ") ")
                    }
                    println(" ")
                    println("app result ("+qRes.size+") : ")
                    for(y <- qRes) {
                      print("("+y._1+","+y._2+ ") ")
                    }
                    println(" ")
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

              val hashFunction = config.dataType.toLowerCase match {
                case "numeric" => "hyperplane"
                case "binary" => "bithash"
              }
              println("Writing results...")
              // Write result as line to file
              resWriter.writeResult({
                val sb = new StringBuilder
                sb.append(config.dataSize+" ")
                sb.append(config.dimensions+" ")
                sb.append(hashFunction+" ")
                sb.append(nodesAddresses.length+" ")
                sb.append(testCase.repsPrNode+" ")
                sb.append(testCase.measure+" ")
                sb.append(testCase.functions+" ")
                sb.append(testCase.probeScheme+" ")
                sb.append(testCase.knn+" ")
                sb.append(testCase.queryMaxCands+" ")
                sb.append(config.warmUpIterations+" ")
                sb.append(avgRecall+" ")
                sb.append(stdDevRecall+" ")
                sb.append((avgTime / 1E6)+"ms ")
                sb.append((stdDevTime / 1E6)+"ms")
                sb.toString
              })

            }
            tcc += 1
          }
          println("Testing has finished")
          lsh.destroy
        }
        case "binary" => {
          println("Running test as 'Binary'")
          // Initialization
          val lsh = new LSHStructure[mutable.BitSet](nodesAddresses)

          // TEST SECTION

          var tcc = 0
          while(tcc < testCases.length) {
            println("Testcase "+(tcc+1)+" out of "+testCases.length+"...")
            val tc = testCases(tcc).split(" ")
            val testCase = TestCase (
              tc(0),        // queriesdir
              tc(1),        // eucqueriesDir (only used when bithash)
              tc(2).toInt,  // repetitions per node
              tc(3).toInt,  // number of functions ( k )
              tc(4),        // probescheme
              tc(5).toInt,  // max cands considered in query before returning
              tc(6),        // similarity measure
              tc(7).toInt, // k nearest neighbors to be tested
              tc(8)        // knnSetsDir dir
            )

            println("Loading knnsets...")
            val knnStructure = loadKNNSets(new File(testCase.knnSetsPath))

            println("Initializing repetitions...")
            val simMeasure = testCase.measure.toLowerCase match {
              case "hamming" => new Hamming(config.dimensions)
              case _ => throw new Exception("Unknown Measure!")
            }

            if(lsh.build(config.data, config.dataeuc, config.dataType, config.dataSize, DisaParserFacBitSet, testCase.repsPrNode, BitHashFactory, testCase.probeScheme, testCase.queryMaxCands, testCase.functions, config.dimensions, simMeasure, rnd.nextLong)) {
              println("LSH repetitions has been initialized..")

              // Get queries, keep last set if fileDir is the same
              if(!testCase.queriesDir.equals(lastQueriesDir)) {
                println("queries has not been loaded. Loading queries...")
                this.bitQueries = DisaParserBinary(Source.fromFile(new File(testCase.queriesDir)).getLines(), config.dimensions).toArray
                this.lastQueriesDir = testCase.queriesDir
              }

              // Get euclidean queries, keep last set if fileDir is the same // Only used for bit hashing
              if(!testCase.eucQueriesDir.equals(lastEucQueriesDir)) {
                println("euc queries has not been loaded. Loading queries...")
                this.eucqueries = DisaParserNumeric(Source.fromFile(new File(testCase.eucQueriesDir)).getLines(), config.dimensions).toArray
                this.lastEucQueriesDir = testCase.eucQueriesDir
              }

              println("Warmup...")
              var i = 0
              while(i < config.warmUpIterations) {
                val index = rnd.nextInt(this.bitQueries.length)
                val qRes = lsh.query(this.bitQueries(index), this.eucqueries(index), testCase.knn)
                if(qRes.nonEmpty) {
                  qRes.head
                }
                i+=1
              }

              println("Running queries...")
              val queryTimes:ArrayBuffer[Double] = ArrayBuffer()
              val queryRecalls:ArrayBuffer[Double] = ArrayBuffer()
              var j = 0
              while(j < this.bitQueries.length) {
                val index = rnd.nextInt(this.bitQueries.length)
                val qp:(Int,mutable.BitSet) = this.bitQueries(index)
                val qpeuc:(Int, Array[Float]) = this.eucqueries(index)
                var qRes: ArrayBuffer[(Int,Double,Int)] = ArrayBuffer()
                var invocationTimes:Array[Double] = new Array(INVOCATION_COUNT)

                // Time Test, every query is made 5 times
                var l = 0
                while(l < INVOCATION_COUNT) {
                  invocationTimes(l) = timer {
                    qRes = lsh.query(qp,qpeuc, testCase.knn)
                  }
                  l+=1
                }

                queryTimes += invocationTimes.sum / INVOCATION_COUNT

                // Recall Test
                val optimalRes = knnStructure(qp._1).take(testCase.knn)

                // Here the recall is the ratio of the sum of distances to qp returned by a query
                queryRecalls += {
                  val optSum:Double = optimalRes.map(_._2).sum
                  var qResSum = qRes.map(x => x._2).sum
                  if(qRes.size < testCase.knn) {
                    // Punishment
                    println("optimal sum: " + optSum)
                    println("punished " + qResSum +" + 10*"+ (testCase.knn - qRes.size))
                    qResSum += 10*(optSum)*(testCase.knn - qRes.size)
                  }
                  if(optSum > qResSum){
                    println(" ")
                    println("opt sum was lt app sum by " + (optSum - qResSum))
                    println("for qp: "+ qp._1)
                    println("opt result ("+optimalRes.length+") : ")
                    for(y <- optimalRes) {
                      print("("+y._1+","+y._2 + ") ")
                    }
                    println(" ")
                    println("app result ("+qRes.size+") : ")
                    for(y <- qRes) {
                      print("("+y._1+","+y._2+ ") ")
                    }
                    println(" ")
                  }

                  optSum / qResSum
                }
                j += 1
              }

              // Queries has been run. Its time to write out results
              val avgTime = queryTimes.sum / this.bitQueries.length
              val stdDevTime:Double = {
                val variance = queryTimes.map(a => math.pow(a - avgTime, 2)).sum / queryTimes.size
                Math.sqrt(variance)
              }

              val avgRecall = queryRecalls.sum / this.bitQueries.length
              val stdDevRecall:Double = {
                val variance = queryRecalls.map(a => math.pow(a - avgRecall, 2)).sum / queryRecalls.size
                Math.sqrt(variance)
              }

              val hashFunction = config.dataType.toLowerCase match {
                case "numeric" => "hyperplane"
                case "binary" => "bithash"
              }
              println("Writing results...")
              // Write result as line to file
              resWriter.writeResult({
                val sb = new StringBuilder
                sb.append(config.dataSize+" ")
                sb.append(config.dimensions+" ")
                sb.append(hashFunction+" ")
                sb.append(nodesAddresses.length+" ")
                sb.append(testCase.repsPrNode+" ")
                sb.append(testCase.measure+" ")
                sb.append(testCase.functions+" ")
                sb.append(testCase.probeScheme+" ")
                sb.append(testCase.knn+" ")
                sb.append(testCase.queryMaxCands+" ")
                sb.append(config.warmUpIterations+" ")
                sb.append(avgRecall+" ")
                sb.append(stdDevRecall+" ")
                sb.append((avgTime / 1E6)+"ms ")
                sb.append((stdDevTime / 1E6)+"ms")
                sb.toString
              })

            }
            tcc += 1
          }
          println("Testing has finished")
          lsh.destroy
        }
        case _ => throw new Exception("Unknown datatype")
      }


    }
    case None => {
     // Do nothing
    }
  }

  def getArgsParser : OptionParser[Config] = {
    new OptionParser[Config]("Recall Test") {
      head("Recall Test", "1.0")

      opt[String]('d', "data").required().valueName("<string>").action((x, c) =>
        c.copy(data = x)).text("input data path!")

      opt[String]('u', "dataeuc").required().valueName("<string>").action((x, c) =>
        c.copy(dataeuc = x)).text("input data euclidean path! (not necessary if it's numeric test)")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(dataSize = x)).text("input data file size")

      opt[Int]('c', "dimensions").required().valueName("<int>").action((x, c) =>
        c.copy(dimensions = x)).text("number of components in a vector")

      opt[String]('t', "dataType").required().valueName("<string>").action((x, c) =>
        c.copy(dataType = x)).text("type of data in each tuple (Int, ???) e.g. (Int, Array[Float]) is 'numeric', (Int, BitSet) is 'binary' ")

      opt[File]('i', "nodesAddresses").required().valueName("<file>").action((x, c) =>
        c.copy(nodes = x)).text("file containing addresses for nodes")

      opt[File]('t', "testCases").required().valueName("<file>").action((x, c) =>
        c.copy(testCases = x)).text("file containing testcases")

      opt[Int]('u', "invocationCount").required().valueName("<int>").action((x, c) =>
        c.copy(invocationCount = x)).text("times each query is run")

      opt[Int]('w', "warmUpIterations").required().valueName("<int>").action((x, c) =>
        c.copy(warmUpIterations = x)).text("How many queries should be run as warmup")

      opt[String]('o', "out").required().valueName("<string>").required().action((x, c) =>
        c.copy(outDir = x)).text("out file directory (without trailing slash")

      opt[Long]('s', "seed").required().valueName("<long>").action((x, c) =>
        c.copy(seed = x)).text("seed for random elements (Long value)")

      help("help").text("prints this usage text")
    }
  }
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
