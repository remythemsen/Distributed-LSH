package multiprobing

/**
  * Created by remeeh on 07-04-2017.
  */
trait ProbeScheme extends Iterator[(Int,Long)] {
  def generate(qp:Array[Float]):Unit
  def hasNext():Boolean
  def next():(Int,Long)
}
