package tools

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
  * Created by remeeh on 5/13/17.
  */
class CandSet(maxCands:Int) {

  var ids:ArrayBuffer[Int] = new ArrayBuffer[Int](maxCands)
  var dists:ArrayBuffer[Double] = new ArrayBuffer[Double](maxCands)
  var distinct:mutable.HashSet[Int] = new mutable.HashSet[Int]()
  var pointer:Int = 0

  def size:Int = this.pointer

  def +=(id:Int, dist:Double) = {
    if(!this.distinct.contains(id)) {
      this.distinct+=id
      if(this.ids.size <= this.pointer) {
        this.ids += id
        this.dists += dist
      } else {
        this.ids.update(this.pointer,id)
        this.dists.update(this.pointer,dist)
      }
      this.pointer+=1
    }
  }

  def nonDistinctAdd(id:Int, dist:Double) : Unit = {
    if(this.pointer == this.ids.size) {
      this.ids.update(this.pointer, id)
      this.dists.update(this.pointer, dist)
    } else {
      this.ids+=id
      this.dists+=dist
    }
    this.pointer+=1
  }

  def <=(dist:Double):Unit = {
    var i = 0
    var tmpPointer = this.pointer
    this.softReset
    while(i < tmpPointer) {
      if(dists(i) <= dist) {
        this.nonDistinctAdd(this.ids(i), this.dists(i))
      }
      i+=1
    }
  }

  def take(n:Int):Unit = {
    if(this.size > n) this.pointer = n-1
  }

  def softReset:Unit = this.pointer = 0
  def reset:Unit = {
    this.distinct.clear()
    this.pointer = 0
  }


}
