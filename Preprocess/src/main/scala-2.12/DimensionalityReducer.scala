import tools.Tools

import scala.util.Random

object DimensionalityReducer{

  def getNewVector(x:Array[Float], matrix:Array[Array[Float]], a: => Array[Float], binary:Boolean, seed:Long, sqrtTargetDim:Double) = {
    if(binary) {
      MatrixVectorProductBit(x,matrix,a, seed)//return Reduced Vector
    } else {
      MatrixVectorProduct(x, matrix, a)
      scaleToTargetDimension(a, sqrtTargetDim)//return Reduced Vector
    }
  }

  def scaleToTargetDimension(a: => Array[Float], sqrtTargetDim:Double) = {
    var i = 0
    while(i < a.length) {
      a(i) = (a(i)/sqrtTargetDim).toFloat
      i+=1
    }
  }

  def getRandMatrix(targetDimensions:Int, originalDimensions:Int, seed:Long): Array[Array[Float]] ={
    val randomMatrix = for {
      i <- (0 until targetDimensions).toArray
      b <- Array(new Array[Float](originalDimensions))
    } yield b

    val rnd = new Random(seed)

    // Populate bMatrix
    for (i <- 0 until targetDimensions) {
      for (j <- 0 until originalDimensions) {
        // dimenstions in each Vector
        randomMatrix(i)(j) = rnd.nextGaussian().toFloat
      }
    }
    //val M=normalizeMatrix(randomMatrix)
    randomMatrix
  }


  def MatrixVectorProduct(x:Array[Float],matrix:Array[Array[Float]], a: => Array[Float])={
    // TODO BUild while init'ing
    var i = 0
    while(i < matrix.length) {
      a(i) = Tools.dotProduct(x, matrix(i)).toFloat
      i+=1
    }
  }

  def MatrixVectorProductBit(x:Array[Float],matrix:Array[Array[Float]], a: => Array[Float], rndSeed:Long) = {
    val rnd = new Random(rndSeed)
    var i = 0
    while(i < matrix.length) {
      a(i) = {
        val dot = Tools.dotProduct(x, matrix(i))
        if(dot < 0) 0
        else if(dot > 0) 1
        else rnd.nextInt(2)
      }
      i+=1
    }
  }
}
