package IO

import concrete.KNN.{KNearestNeighbors, QueryPointId}
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class KNNFileFormatterSpec extends FlatSpec with Matchers {
  "outPut" should "produce to correct output" in {
    val input = HashMap[QueryPointId, KNearestNeighbors](
      (1 -> Array((1, 1.0), (2, 2.0))),
      (2 -> Array((2, 2.0), (3, 3.0)))
    )

    KNNFileFormatter.outPut(input) shouldEqual "1,1 1.0,2 2.0\n2,2 2.0,3 3.0\n"
  }
}
