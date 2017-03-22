package datastructures

import java.util
import hashfunctions.{HashFunction}
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTable(f:() => HashFunction) {
  private val table = new mutable.HashMap[Int, ArrayBuffer[(Int, Array[Float])]]()

  // internal Hash function
  private val hf = f()

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:(Int, Array[Float])) : Unit = {
    val key = util.Arrays.hashCode(hf(v._2))
    val value = {
      if(this.table.contains(key)) this.table(key)++ArrayBuffer(v)
      else ArrayBuffer(v)
    }
    this.table += (key -> value)
  }

  /**
    * @param v a query point
    * @return a list of vectors with same key as v
    */
  def query(v:Array[Float]) : ArrayBuffer[(Int, Array[Float])] = {
    // TODO dont use Array.hashCode
    // TODO optimize
    for {
      p <- hf.generateProbes(v)
      cands <- this.table(util.Arrays.hashCode(p))
    } yield cands
  }
}

