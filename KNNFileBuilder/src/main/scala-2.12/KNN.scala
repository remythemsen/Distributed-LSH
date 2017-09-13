import java.util.concurrent.Executors

import io.Parser.{DisaParser, DisaParserNumeric}
import measures.Distance

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

/**
  * Finds K nearest neighbors of a given set of query points, if the querypoint itself is in the dataset, that will be included
  * in the result set
  */
object KNN {

  def search(data:DisaParserNumeric, queries:DisaParserNumeric, K:Int, distance:Distance[Array[Float]], dataSetSize:Int) = {
    implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(24))

    implicit object Ord extends Ordering[((Int, Array[Float]), Double)] {
      def compare(x: ((Int, Array[Float]), Double), y: ((Int, Array[Float]), Double)) = x._2.compare(y._2)
    }

    println("Building Structure...")
    var progress = 0.0
    var percentile = dataSetSize / 100

    val pqs = new ArrayBuffer[((Int, Array[Float]), mutable.PriorityQueue[((Int, Array[Float]), Double)])]
    while(queries.hasNext) {
      pqs+= Tuple2(queries.next, new mutable.PriorityQueue[((Int, Array[Float]), Double)]()(Ord))
    }

    // Run over dataset once
    while(data.hasNext) {
      val dp = data.next
      val futures: ArrayBuffer[Future[Unit]] = for {
        pq <- pqs
      } yield {
        Future {
          val distFromQ = distance.measure(pq._1._2, dp._2)
          // Update pq if better tuple is found
          if (pq._2.size < K) {
            pq._2.enqueue((dp, distFromQ))
          } else if (pq._2.head._2 > distFromQ) {
            pq._2.dequeue()
            pq._2.enqueue((dp, distFromQ))
          }
        }
      }
      // Await all pq's update
      Await.result(Future.sequence(futures), 20.seconds)

      progress += 1
      if (progress % percentile == 0) {
        println(((progress / dataSetSize) * 100).toInt + "%")
      }
    }
    println("\r" + 100.toString + "%\n")
    println("Returning resultset...")
    val resultSets = new ArrayBuffer[((Int, Array[Float]), Array[((Int, Array[Float]), Double)])]()
    for(pq <- pqs) {
      resultSets += Tuple2(pq._1,pq._2.toArray)
    }
    resultSets
  }
}
