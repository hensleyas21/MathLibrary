import scala.collection.parallel.CollectionConverters._

def LeftRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + i*deltaX)).sum
  sum * deltaX
}

def RightRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + (i+1)*deltaX)).sum
  sum * deltaX
}

def MiddleRiemannSum(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).map(i => f(a + (i+0.5)*deltaX)).sum
  sum * deltaX
}

def LeftRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + i*deltaX)).sum
  sum * deltaX
}

def RightRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + (i+1)*deltaX)).sum
  sum * deltaX
}

def MiddleRiemannSumParallel(f: Double => Double, a:Double, b:Double, n:Int): Double = {
  val deltaX = (b - a)/n
  val sum = (0 until n).par.map(i => f(a + (i+0.5)*deltaX)).sum
  sum * deltaX
}