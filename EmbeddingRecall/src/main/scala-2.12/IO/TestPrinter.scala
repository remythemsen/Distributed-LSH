package IO

import concrete.TestResult

import scala.util.{Failure, Success, Try}

object TestPrinter {
  def outPut(results:Iterator[Try[TestResult]], dataSize:Int, querySize:Int, dimensions:Int, slack:Vector[Double]) : String = {
    "Recall Tests for\n\n" +
      "Dataset of size: " + dataSize.toString + "\n" +
      "Queryset size of: " + querySize + "\n" +
      "Original dimensions: " + dimensions + "\n\n" +
      "Slack:" + slack.foldLeft("")((s, d) => s + " " + d.toString) + "\n\n" +
      results.zipWithIndex.foldLeft("") {
      (finalResult, testResult) => testResult match {
        case (Success(tr), index) => finalResult + index + ": " + tr.toString + "\n"
        case (Failure(e), index) => finalResult + index + ": test failed: " + e.getMessage + "\n"
      }
    }
  }
}
