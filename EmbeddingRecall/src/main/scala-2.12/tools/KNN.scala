package tools

import measures.EuclideanFastDouble

import scala.collection.immutable.HashMap
import scala.collection.mutable

object KNN {
  type PointId = Int
  type QueryPointId = Int
  type DistFromQueryPoint = Double
  type DataPoint = (Int, Array[Double])
  type DataPoints = Iterator[DataPoint]
  type QueryPoints = Iterator[DataPoint]
  type KNearestNeighbors = Array[(PointId, DistFromQueryPoint)]
  type KNNStructure = HashMap[QueryPointId, KNearestNeighbors]


  implicit object Ord extends Ordering[(PointId, DistFromQueryPoint)] {
    def compare(x: (PointId, DistFromQueryPoint), y: (PointId, DistFromQueryPoint)) = x._2.compare(y._2)
  }

  // Each query point will be represented by a priority queue,
  // Then while dataset is parsed over, each point is then compared
  // by each queue(querypoint). Top k closest points from each qp
  // is then returned in a structure

  def generate(k:Int, dps:DataPoints, qps:QueryPoints) : KNNStructure = {
    val queues = qps.foldLeft(Vector[(DataPoint, mutable.PriorityQueue[(PointId, DistFromQueryPoint)])]()){
      (result, queryPoint) => {
        result :+ (queryPoint, mutable.PriorityQueue[(PointId, DistFromQueryPoint)]()(Ord))
      }
    }.par

    dps.foreach((tuple) => {
      queues.foreach (queue => {
        val distFromQ = EuclideanFastDouble.measure(queue._1._2, tuple._2)
        if(queue._2.size < k) queue._2.enqueue((tuple._1, distFromQ))
        else if(queue._2.head._2 > distFromQ) {
          queue._2.dequeue()
          queue._2.enqueue((tuple._1, distFromQ))
        }
      })
    })

    queues.foldLeft(HashMap[QueryPointId, KNearestNeighbors]()) {
      (structure, queryPointWithPQ) =>
        structure + (queryPointWithPQ._1._1 ->
          queryPointWithPQ._2.toArray.sorted(Ord).map(x => (x._1, Math.sqrt(x._2)))
          )
    }
  }

  def populate(lines:Iterator[(Int, Array[(Int, Double)])]) : KNNStructure = {
    lines.foldLeft(HashMap[QueryPointId, KNearestNeighbors]()) {
      (structure, line) =>
        structure + (line._1 -> line._2)
    }
  }
}
