package measures

import tools.Tools._

import scala.collection.mutable

trait Distance[A] {
  def measure(x:A, y:A) : Double
}

object Euclidean extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    var res = 0.0
    var i = 0
    while(i < x.length) {
      res += Math.pow(y(i) - x(i), 2)
      i += 1
    }
    Math.sqrt(res)
  }
}

object Cosine extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    1-(dotProduct(x, y)/(magnitude(x)*magnitude(y))) // TODO remove mag due to unit
  }
}

object CosineUnit extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    (1-dotProduct(x, y)) / 2 //normalizing the result to [0,1]
  }
}

object Hamming extends Distance[mutable.BitSet] {
  def measure(x: mutable.BitSet, y:mutable.BitSet): Double = {
    ???
    // TODO Hamming
  }
}

