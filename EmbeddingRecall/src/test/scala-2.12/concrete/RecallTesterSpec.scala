package concrete

import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap

class RecallTesterSpec extends FlatSpec with Matchers {
  "calcRecall" should "return correct recall" in {
    val expectedRecall = 0.5
    val k = 2

    val foundNeighbors = HashMap(
      5 -> Array((2, 1.0), (4, 2.0), (2, 4.0)),
      6 -> Array((1, 1.0), (4, 2.0), (2, 4.0))
    )
    val optimal = HashMap(
      5 -> Array((1, 1.0), (2, 2.0), (4, 4.0)),
      6 -> Array((2, 1.0), (2, 2.0), (4, 4.0))
    )

    val orgSpaceMap = RecallTester.buildVectorMap(
      orgData = Iterator(
        (1, Array(1.0)),
        (2, Array(2.0)),
        (3, Array(3.0)),
        (4, Array(4.0))
      ),
      orgQueries = Iterator(
        (6, Array(0.0)),
        (5, Array(0.0))
      ),
      optimalResults = optimal
    )

    val slack = Vector(0.0, 0.01, 0.05, 0.1)

    RecallTester.calcRecall(foundNeighbors, optimal, orgSpaceMap, k, slack).foreach(x => {
      assert(x._1 == expectedRecall)
    })

  }
  "countValid" should "count correctly" in {
    val qp = Array(0.0)
    val candidates = Array(
      Array(1.0),
      Array(2.0),
      Array(3.0)
    )
    val kthDist = 3.0
    val slack = 0.0

    assert(RecallTester.countValid(qp, candidates, kthDist, slack) == 3.0)
  }
  "countValid" should "still produce correct output on empty input" in {
    val qp = Array(0.0)
    val candidates = Array.empty[Array[Double]]
    val kthDist = 3.0
    val slack = 0.0

    assert(RecallTester.countValid(qp, candidates, kthDist, slack) == 0.0)
  }
  "buildVectorMap" should "produce structure with correct content" in {
    // DataSet to be picked from
    val data = Iterator(
      (1, Array(1.0)),
      (2, Array(2.0)),
      (3, Array(3.0))
    )

    val queries = Iterator(
      (4, Array(1.0)),
      (5, Array(2.0))
    )

    // We want to pick out items in the DataSet which matches the id's of each item in the arrays in HashMap
    val optimal = HashMap(
      4 -> Array((1, 1.0), (2, 2.0), (3, 3.0)),
      5 -> Array((3, 1.0), (2, 2.0), (1, 3.0))
    )

    val vectorMap = RecallTester.buildVectorMap(data, queries, optimal)

    optimal.foreach(i => {
      i._2.foreach {
        j => assert(vectorMap.contains(j._1))
      }
    })
  }
  "buildVectorMap" should "contain querypoint vectors aswell" in {

    // DataSet to be picked from
    val orgData = Iterator(
      (1, Array(1.0)),
      (2, Array(2.0)),
      (3, Array(3.0))
    )

    val orgQueries = Iterator(
      (4, Array(1.0)),
      (5, Array(2.0))
    )
    // We want to pick out items in the DataSet which matches the id's of each item in the arrays in HashMap
    val optimal = HashMap(
      4 -> Array((1, 1.0), (2, 2.0), (3, 3.0)),
      5 -> Array((3, 1.0), (2, 2.0), (1, 3.0))
    )

    val vectorMap = RecallTester.buildVectorMap(orgData, orgQueries, optimal)

    optimal.foreach(i => {
      assert(vectorMap.contains(i._1))
    })
  }
}
