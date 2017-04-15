package hashfunctions

trait HashFunction {
  def apply(v: Array[Float]): Long
  val state:Array[Array[Float]]
}



