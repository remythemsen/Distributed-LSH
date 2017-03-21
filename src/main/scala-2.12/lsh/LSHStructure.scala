package lsh

import actors.Repetition

/**
  * Structure to be queried on for
  * approximate nearest neighbors
  * stored in its internal hashmaps
  */

class LSHStructure[A](repetitions:IndexedSeq[Repetition]) {

  // Check if reps are ready,
  // then return structure
  // else send init sig and wait

  def query : IndexedSeq[A] = {
    // for each rep, send query, wait for result from all. return set
    ???
  }
}
