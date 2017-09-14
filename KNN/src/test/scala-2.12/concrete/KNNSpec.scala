package concrete

import measures.{EuclideanDouble, Hamming}
import org.scalatest.{FlatSpec, Matchers}

import scala.util.Random

class KNNSpec extends FlatSpec with Matchers {


  "KNN.generate" should "return a structure where knns are sorted ascending" in {
    implicit object Ord extends Ordering[(Int, Double)] {
      def compare(x: (Int, Double), y: (Int, Double)) = x._2.compare(y._2)
    }
    val rnd = new Random(System.currentTimeMillis())
    val measure = EuclideanDouble
    val knn = KNN.generate[Array[Double]] (
      k = 50,
      dataPoints = Iterator.fill(10000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      queryPoints = Iterator.fill(1000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      measure = measure
    )
    assert(knn.forall(x => x._2.sorted(Ord) sameElements x._2))
  }

  "KNN.generate" should "return a structure with no more than specified k in each knn set" in {
    val rnd = new Random(System.currentTimeMillis())
    val k = rnd.nextInt(200)
    val measure = EuclideanDouble
    val knn = KNN.generate(
      k,
      dataPoints = Iterator.fill(10000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      queryPoints = Iterator.fill(1000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      measure = measure
    )
    assert(knn.forall(x => x._2.length <= k))

  }

  "KNN.generate" should "return a non empty structure, if valid queries are provided" in {
    val rnd = new Random(System.currentTimeMillis())
    val measure = EuclideanDouble
    val knn = KNN.generate(
      k = 50,
      dataPoints = Iterator.fill(10000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      queryPoints = Iterator.fill(1000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      measure = measure
    )
    assert(knn.nonEmpty)

  }

  "KNN.generate" should "return at least k specified found knn for each qp if dps (dataset) is > k" in {
    val rnd = new Random(System.currentTimeMillis())
    val measure = EuclideanDouble
    val knn = KNN.generate(
      k = 50,
      dataPoints = Iterator.fill(10000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      queryPoints = Iterator.fill(1000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      measure = measure
    )
    assert(knn.forall(x => x._2.length == 50))

  }

  "KNN.generate" should "return a structure with size equal to |qps| (query points set size)" in {
    val rnd = new Random(System.currentTimeMillis())
    val qpsSize = 1000
    val measure = EuclideanDouble
    val qps = Iterator.fill(qpsSize)((rnd.nextInt, Array.fill(20)(rnd.nextDouble)))
    val knn = KNN.generate(
      k = 50,
      dataPoints = Iterator.fill(10000)((rnd.nextInt, Array.fill(20)(rnd.nextDouble))),
      queryPoints = qps,
      measure = measure
    )

    assert(knn.size == qpsSize)

  }


}
