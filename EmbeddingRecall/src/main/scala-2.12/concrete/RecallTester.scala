package concrete

import measures.EuclideanDouble
import tools.KNN
import tools.KNN.KNNStructure

import scala.collection.immutable.HashMap
import scala.util.{Failure, Success, Try}

object RecallTester {

  // Recall is:
  // The count of points found by KNN in the reduced dataset which has distance to the corresponding query point
  // less than or equal to the k'th farthest point from q in the KNN set done in original space.

  def calcRecall(foundNeighbors:KNNStructure,
                 optimalNeighbors:KNNStructure,
                 orgSpaceSet:HashMap[Int, Array[Double]],
                 k:Int,
                 slack:Vector[Double]
                ) : Vector[(Double, Double)] = {


    val foundNeighborsOrgSpace:Iterable[(Int, Array[Array[Double]])] = foundNeighbors.map(set => (set._1, set._2.map(n => orgSpaceSet(n._1))))
    val initialValues = Vector.fill[Double](slack.size)(0.0).zip(slack)

    foundNeighborsOrgSpace.foldLeft(initialValues) {
      (recallSet, knnSingleSet) =>
        val kthPointDist = optimalNeighbors(knnSingleSet._1)(k-1)._2 // TODO put check in for k > |knn k|

        recallSet.map(recallResult =>
          (countValid(orgSpaceSet(knnSingleSet._1), knnSingleSet._2, kthPointDist, recallResult._2)/optimalNeighbors.size/k, recallResult._2))
    }
  }

  def countValid(queryPoint: Array[Double], candSet:Array[Array[Double]], kthDist:Double, slack:Double) : Double = {
    candSet.foldLeft(0.0) {
      (result, candidate) =>
        if(EuclideanDouble.measure(queryPoint, candidate) <= kthDist * (1.0 + slack)) result + 1.0
        else result
    }

  }

  // Building a map of vectors in original space from which results should
  // be compared in. optimalResults knnstructure is passed in, in order to
  // just load the needed vector into memory from the dataset, the needed
  // vectors are just those appearing in the optimal result sets
  def buildVectorMap(orgData: Iterator[(Int, Array[Double])], orgQueries: Iterator[(Int, Array[Double])], optimalResults:KNNStructure) : HashMap[Int, Array[Double]] = {
    val requiredVectors = optimalResults.foldLeft(Set[Int]()) {
      (set, item) => {
        set + item._1 ++ item._2.map(x => x._1)
      }
    }

    (orgData ++ orgQueries).foldLeft(HashMap[Int, Array[Double]]()) {
      (map, vector) => {
        // Get list of viable items, insert list
        if(requiredVectors.contains(vector._1)) map + (vector._1 -> vector._2)
        else map
      }
    }
  }

  def run(cases:Iterator[Try[TestCase]], optimalSetParser:Iterator[(Int, Array[(Int, Double)])], orgSpaceDataParser:Iterator[(Int, Array[Double])], orgSpaceQueryParser:Iterator[(Int, Array[Double])], slack:Vector[Double]) : Iterator[Try[TestResult]] = {
    cases.map({
      case Success(testCase) => {
        val results = KNN.generate(testCase.knn, testCase.reducedDataParser, testCase.reducedQueryParser)
        val optimalResults = KNN.populate(optimalSetParser)
        val originalSpaceSet = buildVectorMap(orgSpaceDataParser, orgSpaceQueryParser, optimalResults)



        Success({
          TestResult(testCase.dimensions, calcRecall(results, optimalResults, originalSpaceSet, testCase.knn, slack))
        })
      }
      case Failure(throwable) => Failure(throwable)
    })
  }
}
