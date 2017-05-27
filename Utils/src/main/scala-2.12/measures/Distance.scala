package measures

import java.util

import com.googlecode.javaewah.datastructure.BitSet
import org.apache.lucene.util.OpenBitSet
import org.roaringbitmap.RoaringBitmap
import tools.Tools._

trait Distance[A] {
  def measure(x:A, y:A) : Double
}

object EuclideanFast extends Distance[Array[Float]] {
  // Note this is without sqrt!
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
    1.0-(dotProduct(x, y)/(magnitude(x)*magnitude(y)))// TODO remove mag due to unit
  }
}

object CosineUnit extends Distance[Array[Float]] {
  override def measure(x: Array[Float], y: Array[Float]): Double = {
    (1.0-dotProduct(x, y)) / 2.0 //normalizing the result to [0,1]
  }
}

object Hamming2 extends Distance[OpenBitSet] {
  override def measure(x: OpenBitSet, y: OpenBitSet):Double = {
    OpenBitSet.xorCount(x, y)
  }
}

object Hamming extends Distance[BitSet] {
  override def measure(x: BitSet, y: BitSet):Double = {
    x.xorcardinality(y)
  }
}

object Hamming3 extends Distance[util.BitSet] {
  override def measure(x: util.BitSet, y: util.BitSet):Double = {
    val xb:util.BitSet = x.clone().asInstanceOf[util.BitSet]
    xb.xor(y)
    xb.cardinality()
  }
}

object Hamming4 extends Distance[RoaringBitmap] {
  override def measure(x: RoaringBitmap, y: RoaringBitmap):Double = {
    RoaringBitmap.xorCardinality(x,y)//.xor(x, y).getLongCardinality
  }
}
