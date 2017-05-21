package tools

import it.unimi.dsi.fastutil.doubles.DoubleArrayList
import it.unimi.dsi.fastutil.ints.{IntArrayList, IntOpenHashSet}

class CandSet(maxCands:Int) {

  var ids:IntArrayList = new IntArrayList(maxCands)
  var dists:DoubleArrayList = new DoubleArrayList(maxCands)
  var distinct:IntOpenHashSet = new IntOpenHashSet(maxCands)
  var pointer:Int = 1

  def size:Int = this.pointer-1

  def +=(id:Int, dist:Double) = {
    if(this.ids.size < this.pointer) {
      this.ids.add(id)
      this.dists.add(dist)
    } else {
      this.ids.set(this.pointer-1,id)
      this.dists.set(this.pointer-1,dist)
    }
    this.pointer+=1
  }

  def <=(dist:Double):Unit = {
    //Note that in the case of hamming distance, the chance
    //that points close to qp will have identical distances.
    //are much higher than euclidean measure.
    var i = 0
    var tmpPointer = this.pointer - 1
    this.softReset
    while(i < tmpPointer) {
      if(dists.getDouble(i) <= dist) {
        this.+=(this.ids.getInt(i), this.dists.getDouble(i))
      }
      i+=1
    }
  }

  def take(n:Int):Unit = {
    if(this.size > n) this.pointer = n+1
  }

  def softReset:Unit = this.pointer = 1
  def reset:Unit = {
    this.distinct.clear()
    this.pointer = 1
  }


}
