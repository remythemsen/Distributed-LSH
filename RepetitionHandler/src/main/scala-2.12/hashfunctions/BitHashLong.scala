package hashfunctions

import scala.collection.mutable
import scala.util.Random


class BitHashLong(k:Int, dimensions:Int, seed:Long)  {

  private val randomIndices:Array[Int] = generateRandomIndices(dimensions)
  val state: Array[Array[Float]] = ???
  private var result:Long = 0

  def apply(v: mutable.BitSet): Long = {
    ???
/*    var i = 0
    while(i < k) {
      result += (hash(v, randomIndices(i)) << i)
      i += 1
    }
    result*/

  }

  def hash(v: mutable.BitSet, index:Int) : Long = {
    ???
    // result is the bit value at position 'index'
/*    v(index)*/
  }

  def generateProbes(key: Long): Array[Long] = ???

  def generateRandomIndices(dimensions:Int) : Array[Int] = {
    val rnd = new Random(seed)
    val result = Array(k)

    for(i <- 0 until k) {
      result(i) = rnd.nextInt(dimensions)
    }
    result
  }

}
