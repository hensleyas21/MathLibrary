import scala.annotation.targetName

class Vector(val data: List[Double]) {

  def size: Int = data.length

  def magnitude: Double = L2Norm

  @targetName("v_add")
  def +(other: Vector): Vector = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
    new Vector(data.zip(other.data).map { case (a, b) => a + b })
  }

  @targetName("v_subtract")
  def -(other: Vector): Vector = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
      new Vector(data.zip(other.data).map { case (a, b) => a - b })
  }

  @targetName("v_scale")
  def *(scalar: Double): Vector = {
    new Vector(data.map(x => x * scalar))
  }

  def L1Norm: Double = data.map(x => math.abs(x)).sum

  def L2Norm: Double = math.sqrt(data.map(x => x * x).sum)

  def maximumNorm: Double = data.map(x => math.abs(x)).max

  def dotProduct(other: Vector): Double = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
    data.zip(other.data).map { case (a, b) => a * b }.sum
  }

  @TODO
  def crossProduct(otherVectors: List[Vector]): Vector = {
    new Vector([0])
  }

  override def toString: String = data.mkString("<", ", ", ">")

  def toList: List[Double] = data
}