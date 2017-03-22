package measures

import java.util.concurrent.Executors

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

// TODO Factor out into shared lib
trait Distance {
  def measure(x:Array[Float], y:Array[Float]) : Float
}

object Euclidean extends Distance {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    Math.sqrt((x zip y).map {
      case (a, b) => Math.pow(b - a, 2)
    }.sum)
  }
}

object Distance {
  implicit val ec = ExecutionContext.fromExecutorService(Executors.newWorkStealingPool(8))
  def parDotProduct[A:Fractional](x: Array[A], y: Array[A])(implicit num:Numeric[A]): Double = {
    val p = 2
    val futures:ArrayBuffer[Future[A]] = new ArrayBuffer()
    for(i <- 0 until p) {
      futures += Future {
        var r:A = num.zero
        for(j <- i until x.length by p) {
          r = num.plus(r, num.times(x(j),y(j)))
        }
        r
      }
    }
    // Merge
    val results = Await.result(Future.sequence(futures), 5.seconds)
    num.toDouble(results.sum)

  }
}
