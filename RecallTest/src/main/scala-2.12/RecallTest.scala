import java.io.File
import java.text.DecimalFormat

import io.ResultWriter
import scopt.OptionParser

import scala.io.Source

case class TestCase(queriesDir:String, eucQueriesDir:String, repsPrNode:Int, functions:Int, probeScheme:String, queryMaxCands:Int, knnMax:Int, measure:String, knn:Int, knnSetsPath:String)
case class Config(
                   data:String = " ",
                   dataeuc:String = " ",
                   dataSize:Int = 0,
                   dimensions:Int = 128,
                   dataType:String = "numeric",
                   setup:String = "single",
                   nodes:File = new File("."),
                   testCases:File = new File("."),
                   invocationCount:Int = 0,
                   warmUpIterations:Int = 0,
                   outDir:String = "",
                   seed:Long = 1239801l
                 )


object RecallTest extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      val tester = config.dataType.toLowerCase match {
        case "binary" => {
          config.setup.toLowerCase match {
            case "distributed" => new BinaryDistributed(config.data, config.dataeuc, config.dataSize, config.dimensions, config.seed, config.nodes)
            case "single" => new BinarySingle(config.data, config.dataeuc, config.dataSize, config.dimensions, config.seed)
          }
        }
        case "numeric" => {
          config.setup.toLowerCase match {
            case "distributed" => new NumericDistributed(config.data, config.dataSize, config.dimensions, config.seed, config.nodes)
            case "single" => new NumericSingle(config.data, config.dataSize, config.dimensions, config.seed)
          }
        }
      }


      // Making header in testresults file:
      val resWriter = new ResultWriter(config.outDir,"recall-LSH", {
        val sb = new StringBuilder
        sb.append("Test type: " + config.setup + ", " + config.dataType+"\n")
        sb.append("Data file: " + config.data+ " " + config.dataSize + " " + config.dimensions+ "\n")
        sb.append("Warmup Iterations: " + config.warmUpIterations+"\n")
        sb.append("Invocations/Query: " + config.invocationCount+"\n")
        sb.append("\n")
        sb.append("\n")
        sb.append("hf ")
        sb.append("nodes ")
        sb.append("reps/Node ")
        sb.append("meas. ")
        sb.append("k ")
        sb.append("PS ")
        sb.append("knn ")
        sb.append("maxCands ")
        sb.append("knnMax ")
        sb.append("recalls: ")
        sb.append("0.0 ")
        sb.append("± ")
        sb.append("0.001 ")
        sb.append("± ")
        sb.append("0.01 ")
        sb.append("± ")
        sb.append("time ")
        sb.append("± ")
        sb.toString
      })

      val testCases = loadTestCases(Source.fromFile(config.testCases).getLines().toArray)

      // Run each test
      var c = 1
      for(tc <- testCases) {
        println("Running test "+c+" out of "+testCases.length)
        System.gc()
        val res = tester.run(tc, config.warmUpIterations, config.invocationCount)

        val hashFunction = config.dataType.toLowerCase match {
          case "numeric" => "hyperplane"
          case "binary" => "bithash"
        }

        println("Writing results...")
        val df = new DecimalFormat("#.###")
        // Write result as line to file
        resWriter.writeResult({
          val sb = new StringBuilder
          sb.append(hashFunction+" ")
          sb.append({
            if(config.setup.toLowerCase == "single") "1 "
            else Source.fromFile(config.nodes).getLines().length+" "
          })
          sb.append(tc.repsPrNode+" ")
          sb.append(tc.measure.substring(0,3)+" ")
          sb.append(tc.functions+" ")
          sb.append(tc.probeScheme+" ")
          sb.append(tc.knn+" ")
          sb.append(tc.queryMaxCands+" ")
          sb.append(tc.knnMax+" ")
          sb.append(df.format(res._1._1)+" "+df.format(res._2._1)+" ")
          sb.append(df.format(res._1._2)+" "+df.format(res._2._2)+" ")
          sb.append(df.format(res._1._3)+" "+df.format(res._2._3)+" ")
          sb.append(df.format(res._3 / 1E6)+"ms ") // Avg Time
          sb.append(df.format(res._4 / 1E6)+"ms") // Std dev Time
          sb.toString
        })

        c += 1
      }
      println("Testing has finished...")
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

      opt[String]('u', "dataeuc").valueName("<string>").action((x, c) =>
        c.copy(dataeuc = x)).text("input data euclidean path! (not necessary if it's numeric test)")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(dataSize = x)).text("input data file size")

      opt[Int]('c', "dimensions").required().valueName("<int>").action((x, c) =>
        c.copy(dimensions = x)).text("number of components in a vector")

      opt[String]('t', "dataType").required().valueName("<string>").action((x, c) =>
        c.copy(dataType = x)).text("type of data in each tuple (Int, ???) e.g. (Int, Array[Float]) is 'numeric', (Int, BitSet) is 'binary' ")

      opt[String]('u', "setup").required().valueName("<string>").action((x, c) =>
        c.copy(setup = x)).text("'distributed' or 'single'")

      opt[File]('i', "nodesAddresses").valueName("<file>").action((x, c) =>
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
  def loadTestCases(lines:Array[String]) : Array[TestCase] = {
    println("Loading testcases...")
    val testCases = new Array[TestCase](lines.length)
    for(tcl <- lines.indices) {
      val tc = lines(tcl).split(" ")
      testCases(tcl) = TestCase(
        tc(0), // queriesdir
        tc(1), // eucqueriesDir (only used when bithash)
        tc(2).toInt, // repetitions per node
        tc(3).toInt, // number of functions ( k )
        tc(4), // probescheme
        tc(5).toInt, // max cands considered in query before returning
        tc(6).toInt, // knn max (just for bithashing)
        tc(7), // similarity measure
        tc(8).toInt, // k nearest neighbors to be tested
        tc(9) // knnSetsDir dir
      )
    }
    testCases
  }
}
