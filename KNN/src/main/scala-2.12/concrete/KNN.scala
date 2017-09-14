package concrete

import measures.{Distance, EuclideanFastDouble}

import scala.collection.immutable.HashMap
import scala.collection.mutable

object KNN {

  // Each query point will be represented by a priority queue,
  // Then while dataset is parsed over, each point is then compared
  // by each queue(querypoint). Top k closest points from each qp
  // is then returned in a structure

  def generate[A](k:Int, dataPoints:Iterator[(Int, A)], queryPoints:Iterator[(Int, A)], measure:Distance[A]) : HashMap[Int, Array[(Int, Double)]] = {

    implicit object Ord extends Ordering[(Int, Double)] {
      def compare(x: (Int, Double), y: (Int, Double)) : Int = x._2.compare(y._2)
    }

    val queues = queryPoints.foldLeft(Vector[((Int, A), mutable.PriorityQueue[(Int, Double)])]()){
      (result, queryPoint) => {
        result :+ (queryPoint, mutable.PriorityQueue[(Int, Double)]()(Ord))
      }
    }.par

    dataPoints.foreach((tuple) => {
      queues.foreach (queue => {
        val distFromQ = measure.measure(queue._1._2, tuple._2)
        if(queue._2.size < k) queue._2.enqueue((tuple._1, distFromQ))
        else if(queue._2.head._2 > distFromQ) {
          queue._2.dequeue()
          queue._2.enqueue((tuple._1, distFromQ))
        }
      })
    })

    queues.foldLeft(HashMap[Int, Array[(Int, Double)]]()) {
      (structure, queryPointWithPQ) =>
        structure + (queryPointWithPQ._1._1 ->
          queryPointWithPQ._2.toArray.sorted(Ord).map(x => (x._1, Math.sqrt(x._2)))
          )
    }
  }

  def populate(lines:Iterator[(Int, Array[(Int, Double)])]) : HashMap[Int, Array[(Int, Double)]] = {
    lines.foldLeft(HashMap[Int, Array[(Int, Double)]]()) {
      (structure, line) =>
        structure + (line._1 -> line._2)
    }
  }
}
