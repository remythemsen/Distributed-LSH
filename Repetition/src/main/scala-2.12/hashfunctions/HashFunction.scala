package hashfunctions

import measures.Distance
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

trait HashFunction {
  def apply(v: Array[Float]): Array[Int]
  def generateProbes(v : Array[Float]): ArrayBuffer[Array[Int]]
  def flipSign(x:Int):Int = if (x == 0) 1 else 0
}



