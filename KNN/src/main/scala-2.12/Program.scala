import java.io._

import IO.Parser.DisaParserBinary
import IO.{DataParser, KNNFileFormatter}
import concrete.KNN
import measures._
import scopt.OptionParser
import scala.io.Source

object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {

      val qpfile = config.queryPoints
      val dataIterator = Source.fromFile(config.data).getLines()
      val queryIterator = Source.fromFile(config.queryPoints).getLines()

      println(config.measure.toLowerCase match {
        case "euclidean" => {
          KNNFileFormatter.outPut(
            KNN.generate(
              k = config.k,
              dataPoints = DataParser(dataIterator),
              queryPoints = DataParser(queryIterator),
              measure = EuclideanDouble
            )
          )
        }
        case "hamming" => {
          KNNFileFormatter.outPut(
            KNN.generate(
              k = config.k,
              dataPoints = DisaParserBinary(dataIterator, config.dimensions),
              queryPoints = DisaParserBinary(queryIterator, config.dimensions),
              measure = Hamming
            )
          )
        }
      })
    }
    case None => println("Please provide the correct arguments") // Nothing
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

      help("help").text("prints this usage text")
    }
  }
}
