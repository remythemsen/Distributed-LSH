package concrete

import java.io.File

import IO.DataParser

import scala.io.Source
import scala.util.Try

object TestCaseManager {
  def generateCases(cases:Iterator[String]) : Iterator[Try[TestCase]] = {
    cases.map(x => x.split(" ")).map(y => Try(TestCase(
      reducedDataParser = DataParser(Source.fromFile(new File(y(0))).getLines()),
      reducedQueryParser = DataParser(Source.fromFile(new File(y(1))).getLines()),
      knn = y(2).toInt,
      Source.fromFile(new File(y(1))).getLines().next().split(" ").length - 1
    )))
  }
}