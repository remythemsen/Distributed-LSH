package hashfunctions

import scala.collection.mutable
import scala.util.Random
import java.util

import org.apache.lucene.util.OpenBitSet

case class BitHash(k:Int, seed:Long, dimensions:Int) extends HashFunction[OpenBitSet](k, seed, dimensions) {

  private val randomIndices:Array[Int] = generateRandomIndices(dimensions)

  def apply(v: OpenBitSet): Long = {
    var result:Long = 0
    var i = 0
    while(i < k) {
      result += (hash(v, randomIndices(i)) << i)
      i += 1
    }
    result
  }

  def hash(v: OpenBitSet, index:Int) : Int = {
    if(v.get(index)) 1 else 0
  }

  def generateRandomIndices(dimensions:Int) : Array[Int] = {
    val rnd = new Random(seed)
    val result = new Array[Int](k)

    for(i <- 0 until k) {
      result(i) = rnd.nextInt(dimensions)
    }
    result
  }

  override val state: Array[OpenBitSet] = Array(new OpenBitSet())
}
