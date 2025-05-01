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