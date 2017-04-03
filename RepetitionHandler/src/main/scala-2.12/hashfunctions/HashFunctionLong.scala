package hashfunctions

trait HashFunctionLong {
  def apply(v: Array[Float]): Long
  def generateProbes(key: Long): Array[Long]
}



