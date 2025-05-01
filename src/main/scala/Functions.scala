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
 * Computes the Markov chain for a specified length n and returns the resulting vector
 * @param vector - the vector to start with
 * @param matrix - the stochastic matrix
 * @param n - the number of times to multiply the vector by the matrix
 * @return - vector: MyVector
 */
def MarkovChain(vector: MyVector, matrix: Matrix, n: Int): MyVector = {
  val matrixChain: List[Matrix] = List.fill(n)(matrix) // fill() from StackOverflow
  val finalMatrix: Matrix = matrixChain.par.reduce((matrixA, matrixB) => matrixA * matrixB)
  val vectorAsMatrix: Matrix = Matrix(vector.size, 1, List(vector.data))
  (finalMatrix * vectorAsMatrix).getColumn(0)
}