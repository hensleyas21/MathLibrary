import scala.annotation.targetName

class MyVector(val data: List[Double]) {

  def size: Int = data.length

  def magnitude: Double = L2Norm

  @targetName("v_add")
  def +(other: MyVector): MyVector = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
    new MyVector(data.zip(other.data).map { case (a, b) => a + b })
  }

  @targetName("v_subtract")
  def -(other: MyVector): MyVector = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
      new MyVector(data.zip(other.data).map { case (a, b) => a - b })
  }

  @targetName("v_scale")
  def *(scalar: Double): MyVector = {
    new MyVector(data.map(x => x * scalar))
  }

  def L1Norm: Double = data.map(x => math.abs(x)).sum

  def L2Norm: Double = math.sqrt(data.map(x => x * x).sum)

  def maximumNorm: Double = data.map(x => math.abs(x)).max

  def normalize: MyVector = this * (1/magnitude)

  def dotProduct(other: MyVector): Double = {
    if size != other.size then throw new UnsupportedOperationException("Vectors must be the same size") else
    data.zip(other.data).map { case (a, b) => a * b }.sum
  }

  def crossProduct(otherVectors: List[MyVector]): MyVector = if otherVectors.length != size-2 then throw new UnsupportedOperationException("There are not the right number of vectors given") else {
    val vectorList: List[MyVector] = new MyVector(List.fill(size)(0)) :: this :: otherVectors
    val matrix: SquareMatrix = new SquareMatrix(vectorList)
    new MyVector((0 until size).map(i => matrix.getSubMatrix(0, i).determinant * (if i%2==0 then 1 else -1)).toList)
  }

  override def toString: String = data.mkString("<", ", ", ">\n")

  def toList: List[Double] = data
}