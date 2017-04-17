package hashfunctions

abstract class HashFunction[A](k: Int, seed: Long, numOfDim:Int) {
  def apply(v: A): Long
  val state:Array[A]
}



