package tools

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import measures.EuclideanFast

import scala.collection.mutable

object Tools {

  def dotProduct(x:Array[Float], y:Array[Float]) : Double = {
    var i = 0
    var res:Double = 0.0
    while(i < x.length) {
      res += x(i) * y(i)
      i+=1
    }
    res
  }

  def dotProduct[A](x:A, y:A) : Double = {
    // Not great!! TODO find a better solution
    var i = 0
    var res:Double = 0.0
    while(i < x.asInstanceOf[Array[Float]].length) {
      res += (x.asInstanceOf[Array[Float]](i) * y.asInstanceOf[Array[Float]](i))
      i+=1
    }
    res
  }

  def magnitudeD(x:Array[Double]) : Double = {
    var r = 0.0
    var i = 0
    while(i < x.length) {
      r+= x(i)*x(i)
      i+=1
    }
    Math.sqrt(r)
  }

  def magnitude(x: Array[Float]) : Double = {
    var r:Double = 0.0
    var i = 0
    while(i < x.length) {
      r+= x(i)*x(i)
      i += 1
    }

    Math.sqrt(r)
  }

  implicit object PQOrd extends Ordering[Int] {
    var dists:DoubleArrayList = _
    def compare(x: Int, y: Int) = dists.getDouble(x).compare(dists.getDouble(y))
  }

  def knn(cands:CandSet, eucDataSet:Array[Array[Float]], lookUpMap:Array[Int], pq:mutable.PriorityQueue[Int], qp:Array[Float], k:Int) : Unit = {
    // Assuming that cands has size >= k

    PQOrd.dists = null
    PQOrd.dists = cands.dists

    var l = 0
    // fill up the queue with initial data
    while(pq.size < k) {
      cands.dists.set(l, EuclideanFast.measure(eucDataSet(cands.ids.getInt(l)), qp))
      pq.enqueue(l)
      l+=1
    }

    while(l < cands.size) {
      cands.dists.set(l, EuclideanFast.measure(eucDataSet(cands.ids.getInt(l)), qp))

      if(cands.dists.getDouble(l) < cands.dists.getDouble(pq.head)) {
        pq.dequeue()
        pq.enqueue(l)
      }
      l+=1
    }

    // resetting counter of cands to make sure we only consider k from this point
    cands.softReset
    val tmpIds = new Array[Int](k)
    val tmpDists = new Array[Double](k)
    var v,w,candsIndex = 0
    while(pq.nonEmpty) {
      candsIndex = pq.dequeue()
      tmpIds(v) = lookUpMap(cands.ids.getInt(candsIndex))
      tmpDists(v) = cands.dists.getDouble(candsIndex)
      v+=1
    }
    var m = 0
    while(m < tmpIds.length) {
      cands+=(tmpIds(m), tmpDists(m))
      m+=1
    }
  }

}
