import scala.io.Source

object Program extends App {
  Config().getArgsParser.parse(args, Config()) match {
    case Some(conf) => {
      println(outPut(runTests(conf)))
    }
    case None => // do nothing
  }

  def runTests(conf:Config) : List[Either[String, TestResult]] = {
    // We need some files
      // testcases
    @annotation.tailrec
    def run(testCases: List[Either[String,TestCase[Array[Float]]]], results:List[Either[String, TestResult]]) : List[Either[String, TestResult]] = testCases match {
       case Left(e) :: xs => run(xs, Left(e) :: results) // could write 'testcase' could not be loaded
       case Right(testCase) :: xs => run(xs, Right {


         // Make test results from the testcase... (run it!)
         TestResult(List(Right(0.5),Right(2.0))) // Hard work here



       } :: results)
       case _ => results
    }
    run(TestManager.loadTestCases(Source.fromFile(conf.testCases).getLines()), List())
  }

  def outPut(results:List[Either[String, TestResult]]) : String = {
    @annotation.tailrec
    def go(lines:List[Either[String, TestResult]], res:String) : String =  {
      lines match {
        case Left(e) :: xs => go(xs, res + "Error: " + e + "\n")
        case Right(tr) :: xs => go(xs, res + tr.toString + "\n")
        case _ => res
      }
    }
    go(results, "")
  }

  case class TestResult(recall:List[Either[String,Double]]) {
    override def toString(): String = {
      @annotation.tailrec
      def go(list:List[Either[String,Double]], res:String):String = {
        list match {
          case Left(_) :: xs => go(xs, res + "NaN" + " ")
          case Right(x) :: xs => go(xs, res + x.toString + " ")
          case _ => res
        }
      }
      go(recall, "")
    }
  }


  def preTesting() : Unit = {
    // Prepare for testing...
    println("preparing for testing...")

    // load in original Dataset

    // run tests

    // output results
  }


}
