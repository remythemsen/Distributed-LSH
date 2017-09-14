import java.io._

import IO.{DataParser, KNNFileFormatter}
import concrete.KNN
import measures.{Cosine, CosineUnit, Distance, Euclidean}
import scopt.OptionParser

import scala.collection.mutable
import scala.io.Source

object Program extends App {

  getArgsParser.parse(args, Config()) match {
    case Some(config) => {
      val measure = config.measure.toLowerCase match {
        case "euclidean" => Euclidean
      }

      val qpfile = config.queryPoints

      println(
        KNNFileFormatter.outPut(
          KNN.generate(
            k = config.k,
            dps = DataParser(Source.fromFile(config.data).getLines),
            qps = DataParser(Source.fromFile(config.queryPoints).getLines)
          )
        )
      )

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
