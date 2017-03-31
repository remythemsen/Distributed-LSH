package measures

import java.util.concurrent.Executors

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}

// TODO Factor out into shared lib
trait Distance {
  def measure(x:Array[Float], y:Array[Float]) : Double
}

object EuclideanOld extends Distance {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    Math.sqrt((x zip y).map {
      case (a, b) => Math.pow(b - a, 2)
    }.sum)
  }
}

object Euclidean extends Distance {
  // TODO Consider single left shift instead
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    var res = 0.0
    var i = 0
    while(i < x.length) {
      res += Math.pow(y(i) - x(i), 2)
      i += 1
    }
    res
  }
}


object Cosine extends Distance {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    1-(Distance.dotProduct(x, y)/((Distance.magnitude(x)*Distance.magnitude(y)))) // TODO remove mag due to unit
  }
}

object Distance {
  def dotProduct(x:Array[Float], y:Array[Float]) : Float = {
    var i = 0
    var res:Float = 0f
    while(i < x.length) {
      res += x(i) * y(i)
      i+=1
    }
    res
  }

  def magnitude(x: Array[Float]) : Double = {
    var r:Double = 0.0
    var i = 0
    while(i < x.length) {
      r+=Math.pow(x(i), 2)
      i += 1
    }

    Math.sqrt(r)
  }

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
