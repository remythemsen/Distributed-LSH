package datastructures

import java.util

import hashfunctions.HashFunction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class ProbeTableLongMap(hashFunction: HashFunction) {
  private val table = new mutable.LongMap[ArrayBuffer[(Int, Array[Float])]]()

  // internal Hash function
  private val hf = hashFunction

  /**
    * Insert vector
    * @param v vector to be inserted into internal hashmap
    */
  def +=(v:(Int, Array[Float])) : Unit = {
    val key = toLong(hf(v._2))
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
    val results = new ArrayBuffer[(Int, Array[Float])]
    val probes = hf.generateProbes(hf(v))
    var i = 0
    while(i < probes.length) {
      results ++= this.table.getOrElse(toLong(probes(i)), ArrayBuffer())
      i+=1
    }
    results
  }

  def toLong(key:Array[Int]):Long = {
    var i = 0
    var long = 0
    while(i < key.length) {
      long += key(i)
      long = long << 1
      i+=1
    }
    long
  }
}

