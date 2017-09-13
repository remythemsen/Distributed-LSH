import java.io.File

import scopt.OptionParser

case class Config(
                   data:File = new File(""),
                   dataSize:Int = 0,
                   dimensions:Int = 128,
                   dataFormat:String = "yfc",
                   queryPoints:File = new File("."),
                   querySize:Int = 0,
                   knnstructure:File = new File("."),
                   knn:Int = 50,
                   testCases:File = new File("."),
                   measure:String = "euclidean",
                   outDir:String = ""
                 ) {

  def getArgsParser : OptionParser[Config] = {
    new OptionParser[Config]("Embedding Recall Test") {
      head("Embedding Recall Test", "1.0")

      opt[File]('d', "data").required().valueName("<File>").action((x, c) =>
        c.copy(data = x)).text("Datafile before any embedding has been applied.")

      opt[Int]('n', "size").required().valueName("<int>").action((x, c) =>
        c.copy(dataSize = x)).text("input data file size")

      opt[Int]('x', "dimensions").required().valueName("<int>").action((x, c) =>
        c.copy(dimensions = x)).text("|components| in each descriptor (in original dataset)")

      opt[String]('f', "dataFormat").required().valueName("<string>").action((x, c) =>
        c.copy(dataFormat = x)).text("Datafile before any embedding has been applied.")

      opt[File]('q', "queryPoints").required().valueName("<file>").action((x, c) =>
        c.copy(queryPoints = x)).text("query points before any embedding has been applied.(must match ids' in querypoints provided in testcases")

      opt[Int]('z', "querySize").required().valueName("<int>").action((x, c) =>
        c.copy(querySize = x)).text("Size of queryset")

      opt[Int]('d', "dimensions").required().valueName("<int>").action((x, c) =>
        c.copy(dimensions = x)).text("number of components in each vector")

      opt[Int]('k', "knn").required().valueName("<int>").action((x, c) =>
        c.copy(knn = x)).text("K Neighbors to be found for each query")

      opt[File]('t', "testCases").required().valueName("<file>").action((x, c) =>
        c.copy(testCases = x)).text("testcases to be processed")

      opt[String]('m', "measure").required().valueName("<string>").action((x, c) =>
        c.copy(measure = x)).text("similarity measure to be used for comparison")

      opt[String]('o', "outDir").required().valueName("<string>").action((x, c) =>
        c.copy(outDir = x)).text("directory path for result files")

      help("help").text("prints this usage text")
    }
  }
}

