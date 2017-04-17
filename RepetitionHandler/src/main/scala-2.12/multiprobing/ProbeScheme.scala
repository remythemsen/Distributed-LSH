package multiprobing

/**
  * Created by remeeh on 07-04-2017.
  */
trait ProbeScheme[A] extends Iterator[(Int,Long)] {
  def generate(qp:A):Unit
  def hasNext():Boolean
  def next():(Int,Long)
}
