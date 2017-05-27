package hashfunctions

import scala.util.Random

import com.googlecode.javaewah.datastructure.BitSet


case class BitHash(k:Int, seed:Long, dimensions:Int) extends HashFunction[BitSet](k, seed, dimensions) {

  private val randomIndices:Array[Int] = generateRandomIndices(dimensions)

  def apply(v: BitSet): Long = {
    var result:Long = 0
    var i = 0
    while(i < k) {
      result += (hash(v, randomIndices(i)) << i)
      i += 1
    }
    result
  }

  def hash(v: BitSet, index:Int) : Int = {
    if(v.get(index)) 1 else 0
  }

  def generateRandomIndices(dimensions:Int) : Array[Int] = {
    println("Bithash with seed: " + seed)
    val rnd = new Random(seed)
    val result = new Array[Int](k)

    for(i <- 0 until k) {
      result(i) = rnd.nextInt(dimensions)
    }
    result
  }

  override val state: Array[BitSet] = Array(new BitSet())
}
