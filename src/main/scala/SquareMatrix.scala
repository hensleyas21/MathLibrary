import scala.annotation.targetName
import scala.collection.parallel.CollectionConverters._

class SquareMatrix (n: Int, data: List[List[Double]]) extends Matrix (n, n, data) {
  
  private var det: Double = 0.0

  def this(vectorList: List[MyVector]) = this(vectorList.size, vectorList.map(v => v.toList))

  def determinant: Double = if n == 1 then data.head.head else (0 until n).map(i => getSubMatrix(0, i).determinant * data.head(i) * (if i%2==0 then 1 else -1)).sum

  def determinantParallel: Double = if n == 1 then data.head.head else (0 until n).par.map(i => getSubMatrix(0, i).determinantParallel * data.head(i) * (if i%2==0 then 1 else -1)).sum

  def isInvertible: Boolean = determinant != 0

  def isInvertibleParallel: Boolean = determinantParallel != 0

  @targetName("m_add")
  override def +(other: Matrix): SquareMatrix = super.+(other).toSquareMatrix

  @targetName("m_subtract")
  override def -(other: Matrix): SquareMatrix = super.-(other).toSquareMatrix

  @targetName("m_scale")
  override def *(scalar: Double): SquareMatrix = super.*(scalar).toSquareMatrix

  @targetName("m_product")
  def *(other: SquareMatrix): Matrix = super.*(other).toSquareMatrix

  override def transpose: SquareMatrix = super.transpose.toSquareMatrix

  override def getSubMatrix(n: Int, m: Int): SquareMatrix = super.getSubMatrix(n, m).toSquareMatrix

  def adjoint: SquareMatrix = {
    val cofactorMatrix = (0 until n).map(i => (0 until n).map(j => {
        val minor = getSubMatrix(i, j).determinant
        val sign = if ((i + j) % 2 == 0) 1 else -1
        sign * minor
      }).toList).toList
    new SquareMatrix(n, cofactorMatrix).transpose
  }

  def adjointParallel: SquareMatrix = {
    val cofactorMatrix = (0 until n).par.map(i => (0 until n).map(j => {
      val minor = getSubMatrix(i, j).determinant
      val sign = if ((i + j) % 2 == 0) 1 else -1
      sign * minor
    }).toList).toList
    new SquareMatrix(n, cofactorMatrix).transpose
  }

  def inverse: SquareMatrix = if !isInvertible then throw new UnsupportedOperationException("Matrix must have non-zero determinant") else adjoint * (1/determinant)

  def inverseParallel: SquareMatrix = if !isInvertibleParallel then throw new UnsupportedOperationException("Matrix must have non-zero determinant") else adjointParallel * (1/determinantParallel)
}
