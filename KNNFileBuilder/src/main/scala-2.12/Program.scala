import java.io._

import io.Parser.{DisaParser, DisaParserNumeric}
import measures.{Cosine, CosineUnit, Distance, Euclidean}
import scopt.OptionParser

import scala.collection.mutable
import scala.io.Source

/**
  * Created by remeeh on 14-03-2017.
  */
object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      val measure = config.measure.toLowerCase match {
        case "cosine" => Cosine
        case "euclidean" => Euclidean
        case "cosineunit" => CosineUnit
      }

      val knnSName = config.outDir + "/"+config.data.getName.substring(0,config.data.getName.length-5)+"-"+config.measure.toLowerCase+".knn"
      val qpfile = config.queryPoints
      buildKNNStructure(new File(knnSName), DisaParserNumeric(Source.fromFile(config.data).getLines(), config.dimensions), DisaParserNumeric(Source.fromFile(qpfile).getLines(), config.dimensions), config.k, measure, config.n)

    }
    case None => // Nothing
  }


  def buildKNNStructure(file:File, optData: DisaParserNumeric, queries: DisaParserNumeric, K: Int, measure: Distance[(Array[Float])], N: Int): mutable.HashMap[Int, Array[(Int, Double)]] = {
    val structure = new mutable.HashMap[Int, Array[(Int, Double)]]
    val resultSets = KNN.search(optData, queries, K, measure, N)


    for (rSet <- resultSets) {
      structure += (rSet._1._1 -> rSet._2.sortBy(x => x._2).map(x => (x._1._1, x._2)))
    }

    println("Saving structure to disk...")
    write(file, structure)
    println("structure was saved..")

    structure
  }


  def write(file:File, map:mutable.HashMap[Int, Array[(Int, Double)]]): Unit = {
    val arr:Array[(Int, Array[(Int, Double)])] = map.toArray
    for(i <- 0 until arr.length) {
      val res:String = {
        arr(i)._1.toString + {
          val sb = new StringBuilder
          for(x <- arr(i)._2) {
            sb.append("," + x._1.toString + " " + x._2.toString)
          }
          sb.toString
        }
      }
      writeResult(res)
    }


    def writeResult(line:String) : Unit = {
      val bw = new BufferedWriter(new FileWriter(file,true))
      bw.append(line+"\n")
      bw.close()
    }
  }

  def getArgsParser : OptionParser[Config] = {
    new OptionParser[Config]("KNNBuilder") {
      head("KNNBuilder", "1.0")

      opt[File]('d', "data").required().valueName("<file>").action((x, c) =>
        c.copy(data = x)).text("input data file!")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(n = x)).text("input data file size")

      opt[Int]('c', "dimensions").required().valueName("<int>").action((x, c) =>
        c.copy(k = x)).text("number of components in a vector")

      opt[Int]('k', "knn").required().valueName("<int>").action((x, c) =>
        c.copy(k = x)).text("input data file size")

      opt[File]('q', "querypoints").required().valueName("<file>").action((x, c) =>
        c.copy(queryPoints = x)).text("optimal dataset query points")

      opt[String]('m', "measure").valueName("<string>").required().action((x, c) =>
        c.copy(measure = x)).text("measure chosen for optimal set generation")

      opt[String]('o', "out").valueName("<string>").required().action((x, c) =>
        c.copy(outDir = x)).text("out file directory (without trailing slash")

      help("help").text("prints this usage text")
    }
  }
}
