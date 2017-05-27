package multiprobing

trait ProbeScheme[A] extends Iterator[(Int,Long)] {
  def generate(qp:A):Unit
  def hasNext():Boolean
  def next():(Int,Long)
}
