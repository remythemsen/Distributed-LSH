package hashfunctions

import scala.collection.mutable.ArrayBuffer

trait HashFunction {
  def apply(v: Array[Float]): Array[Int]
  def generateProbes(v : Array[Float]): ArrayBuffer[Array[Int]]
}



