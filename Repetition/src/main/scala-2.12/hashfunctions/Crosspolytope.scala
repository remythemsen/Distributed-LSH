package hashfunctions

import measures.Distance

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// TODO This needs to be optimized and tested

case class Crosspolytope(k: Int, seed:Long, numOfDim: Int) extends HashFunction {
  // Initialization
  private val rnd:Random = new Random(seed)

  private val rotations = new Array[Array[Float]](k)
  private val arrayOfMaxIndices = new Array[Int](k)

  def generateRDV(size:Int, seed:Long) : Array[Float] = {
    val rnd = new Random(seed)
    val diagonalMatrix = new Array[Float](size)
    for(i <- 0 until size) {
      if(rnd.nextBoolean()) diagonalMatrix(i) = -1
      else diagonalMatrix(i) = 1
    }
    diagonalMatrix
  }

  private val diagonals:Array[Array[Float]] = for {
    i <- (0 until k * 3).toArray // 3 diagonals for each k
    x <- Array(generateRDV(numOfDim, rnd.nextLong))
  } yield x


  override def apply(x: Array[Float]): Array[Int] = {

    val b = new Array[Float](numOfDim)
    val H = hadamardTransformation(x, 0, numOfDim-1, b)
    val hashcode = new Array[Int](k)
    var index = 0


    for (i <- 0 until k) {
      val y = pseudoRandomRotation(H, x, index)
      // Rotations live on global var for multiprobing access
      rotations(i) = y

      var max = 0.0
      var indexOfMax = 0
      for (i <- 0 until numOfDim) {
        if (Math.abs(y(i)) > max) {
          max = y(i)
          indexOfMax = i
        }
      }

      // generate hashing value for each rotation
      if (max > 0) hashcode(i) = 2 * indexOfMax - 1
      else hashcode(i) = 2 * indexOfMax - 2

      arrayOfMaxIndices(i) = indexOfMax

      index += 3
    }

    hashcode
  }

  private def hadamardTransformation(a: Array[Float], low: Int, high: Int, y: Array[Float]): Array[Float]={
    if(high - low > 0) {
      val middle = (low + high) / 2
      var c = 1
      for(i <- low until middle + 1){
        y(i) = a(i) + a(middle + c)
        c += 1
      }

      var m = 0
      for(j <- middle + 1 until high + 1){
        y(j) = -a(j) + a(low + m)
        m += 1
      }
      val b = new Array[Float](a.length)
      for(i <- a.indices){
        b(i) = y(i)
      }

      // recursively call the Hadamard transformation method on the 2 halves
      // TODO
      hadamardTransformation(b, low, middle, y)
      hadamardTransformation(b, middle + 1, high, y)
    }
    y
  }

  private def pseudoRandomRotation(H: Array[Float], x: Array[Float], i: Int): Array[Float] ={
    def VectorMultiplication(A: Array[Float], x: Array[Float]): Array[Float] = {
      val b = new Array[Float](numOfDim)
      for(i <- 0 until numOfDim){
        b(i) = A(i) * x(i)
      }
      b
    }

    VectorMultiplication(H,
      VectorMultiplication(diagonals(i),
        VectorMultiplication(H,
          VectorMultiplication(diagonals(i + 1),
            VectorMultiplication(H,
              VectorMultiplication(diagonals(i + 2), x))))))
  }

  // TODO How many
  private val probes:Array[Array[Int]] = {
    val a = new Array[Array[Int]]((k * (k + 1) / 2) + 1) // Array of probes to be reused
    for (i <- 0 until a.length) {
      a(i) = new Array[Int](k)
    }
    a
  }

  private implicit object Ord extends Ordering[(IndexedSeq[Int],Float)] {
    def compare(x:(IndexedSeq[Int],Float), y:(IndexedSeq[Int],Float)):Int = y._2.compare(x._2)
  }

  private var pq:mutable.PriorityQueue[(IndexedSeq[Int], Float)] = new mutable.PriorityQueue[(IndexedSeq[Int], Float)]()


  // M = # of rotations (functions)
  private val M = rotations.length

  // pairsLists = lists of pairs of distances and indices to the max value, for each CP
  private val pairsLists = new Array[Array[(Float,Int)]](M)

  override def generateProbes(hashCode: Array[Int]): Array[Array[Int]] = {
    // TODO Find amount
    val numOfProbes = 150

    // TODO update to long
    val pertSetList = generateSets(numOfProbes)

    // listBuckets = list of probing buckets

    val T = pertSetList.length
    val listOfProbingBuckets = new ArrayBuffer[Array[Int]](T)
    for(i<-0 until T){
      val probingBucket = new Array[Int](M)
      for(j<-0 until M){
        // get the right CP
        val listOfPairs = pairsLists(j)

        // retrieve the corresponding pair
        val pair = listOfPairs(pertSetList(i)(j))

        // get the index of the next closest point to the query
        val indexInVector = pair._2

        // compute the hash values of the probing bucket
        if(rotations(j)(indexInVector) < 0){
          probingBucket(j) = 2 * indexInVector - 2
        } else {
          probingBucket(j) = 2 * indexInVector - 1
        }
      }
      listOfProbingBuckets += probingBucket
    }
    listOfProbingBuckets.toArray
  }

  private def generateSets(Tsize:Int):Array[IndexedSeq[Int]]={

    def score(a: IndexedSeq[Int]): Float = {
      // returns the score of a perturbation set
      var score=0.0f

      for(i<- a.indices) {
        val curList = pairsLists(i)
        val pair = curList(a(i))
        val dist = pair._1
        score += dist*dist
      }
      score
    }

    def shift(A: IndexedSeq[Int]): IndexedSeq[Int] = {
      // replaces last element of A by 1 + the element's value
      val index = A.size-1
      val newVal = A(index)+1
      val B = A.updated(index, newVal)
      B
    }

    def expand(A: IndexedSeq[Int]): IndexedSeq[Int] = {
      // adds the last element + 1 to the set
      val B = A:+0
      B
    }

    val setsList = new Array[IndexedSeq[Int]](Tsize)

    // initialize the heap with the element 0, with the score of 0
    pq.enqueue((IndexedSeq(0),0.0f))

    // i counts the number of sets that are added to the setsList
    var i = 0
    var done = false

    do{
      // extract the element of minimum score from the heap
      val ps = pq.dequeue()._1

      // when perturbation set "complete", add to the setsList
      if(ps.size == M){
        setsList(i) = ps

        // add the shifted perturbation set to the heap
        val shiftedPs = shift(ps)
        val scoreShiftedPs = score(shiftedPs)
        pq.enqueue((shiftedPs, scoreShiftedPs))

        if(i < Tsize - 1){
          i += 1
        } else {
          done = true
        }
      } else {
        // add the shifted perturbation set to the heap
        val shiftedPs = shift(ps)
        val scoreShiftedPs = score(shiftedPs)
        pq.enqueue((shiftedPs, scoreShiftedPs))

        // add the expanded perturbation set to the heap
        val expandedPs = expand(ps)
        val scoreExpandedPs = score(expandedPs)
        pq.enqueue((expandedPs, scoreExpandedPs))
      }
    } while( !done )

    setsList
  }
}
