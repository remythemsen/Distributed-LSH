package datastructures

import java.util

import hashfunctions.{HashFunction, Hyperplane}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTable(hashFunction: HashFunction) {
  private val table = new mutable.HashMap[Int, ArrayBuffer[(Int, Array[Float])]]()

  // internal Hash function
  private val hf = hashFunction

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
    // TODO dont append arrays!
    // TODO optimize
    val results = new ArrayBuffer[(Int, Array[Float])]
    val probes = hf.generateProbes(hf(v))
    var i = 0
    while(i < probes.length) {
      results ++= this.table.getOrElse(util.Arrays.hashCode(probes(i)), ArrayBuffer())
      i+=1
    }
    results
  }
}

