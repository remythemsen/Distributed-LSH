import IO.{DataParser, KNNFileParser, TestFormatter}
import concrete.{RecallTester, TestCaseManager}

import scala.io.Source

object Program extends App {
  // Process IO and run embedding recall test
  Config().getArgsParser.parse(args, Config()) match {
    case Some(conf) => {

      val slack = Vector(0.00, 0.01, 0.05, 0.1)

      // Run tests
      val results = RecallTester.run(
        cases = TestCaseManager.generateCases(Source.fromFile(conf.testCases).getLines()),
        optimalSetParser = KNNFileParser(Source.fromFile(conf.knnstructure).getLines()),
        orgSpaceDataParser = DataParser(Source.fromFile(conf.data).getLines()),
        orgSpaceQueryParser = DataParser(Source.fromFile(conf.queryPoints).getLines()),
        slack
      )

      println(TestFormatter.outPut(results, conf.dataSize, conf.querySize, conf.dimensions, slack))
    }
    case None => {
      // do nothing
      println("Please provide the set of required arguments!")
    }
  }
}
