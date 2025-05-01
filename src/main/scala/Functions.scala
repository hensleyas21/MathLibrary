import scala.collection.parallel.CollectionConverters._

def leftRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + i*deltaX)).sum
  sum * deltaX
}

def rightRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + (i+1)*deltaX)).sum
  sum * deltaX
}

def middleRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + (i+0.5)*deltaX)).sum
  sum * deltaX
}

def leftRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + i*deltaX)).sum
  sum * deltaX
}

def rightRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + (i+1)*deltaX)).sum
  sum * deltaX
}

def middleRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + (i+0.5)*deltaX)).sum
  sum * deltaX
}

/**
 * Computes the Markov chain sequentially for a specified length n and returns the resulting vector
 * @param vector the vector to start with
 * @param matrix the stochastic matrix
 * @param n the number of times to multiply the vector by the matrix
 * @return the resulting vector
 */
def MarkovChain(vector: MyVector, matrix: Matrix, n: Int): MyVector = {
  val newMatrix: SquareMatrix = SquareMatrix(matrix.n, matrix.data) // will throw an exception if cannot be made into a square matrix
  val matrixChain: List[Matrix] = List.fill(n)(matrix) // fill() from StackOverflow
  val finalMatrix: Matrix = matrixChain.reduce((matrixA, matrixB) => matrixA * matrixB)
  val vectorAsMatrix: Matrix = Matrix(1, vector.size, List(vector.data)).transpose
  val finalVector: MyVector = (finalMatrix * vectorAsMatrix).getColumn(0)
  // println(Matrix(1, finalVector.size, List(finalVector.data)).transpose)
  finalVector
}

/**
 * Computes the Markov chain in parallel for a specified length n and returns the resulting vector
 * @param vector the vector to start with
 * @param matrix the stochastic matrix
 * @param n the number of times to multiply the vector by the matrix
 * @return the resulting vector
 */
def ParMarkovChain(vector: MyVector, matrix: Matrix, n: Int): MyVector = {
  val newMatrix: SquareMatrix = SquareMatrix(matrix.n, matrix.data) // will throw an exception if cannot be made into a square matrix
  val matrixChain: List[Matrix] = List.fill(n)(matrix) // fill() from StackOverflow
  val finalMatrix: Matrix = matrixChain.par.reduce((matrixA, matrixB) => matrixA * matrixB)
  val vectorAsMatrix: Matrix = Matrix(1, vector.size, List(vector.data)).transpose
  val finalVector: MyVector = (finalMatrix * vectorAsMatrix).getColumn(0)
  // println(Matrix(1, finalVector.size, List(finalVector.data)).transpose)
  finalVector
}