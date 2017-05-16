package measures

import tools.Tools._
import java.util.BitSet

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
    // Remove the sqrt, only apply when it is needed (when comparing to distances obtained from somewhere else.)
    Math.sqrt(res)
  }
}

object Cosine extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    1.0-(dotProduct(x, y)/(magnitude(x)*magnitude(y)))// TODO remove mag due to unit
  }
}

object CosineUnit extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    (1.0-dotProduct(x, y)) / 2.0 //normalizing the result to [0,1]
  }
}
class HammingJava extends Distance[BitSet] {

  override def measure(x: BitSet, y: BitSet):Double = {
    val xb:BitSet = x.clone().asInstanceOf[BitSet]
    xb.xor(y)
    xb.cardinality()
  }
}

class Hamming(dimensions:Int) extends Distance[mutable.BitSet] {
  override def measure(x: mutable.BitSet, y:mutable.BitSet): Double = {

    var i = 0
    var res:Double = 0.0
    while(i < dimensions) {
      if(x(i) != y(i)) res+=1
      i+=1
    }
    res

  }
}

