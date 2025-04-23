import scala.annotation.targetName

class SquareMatrix (n: Int, data: List[List[Double]]) extends Matrix (n, n, data) {

  // TODO: finish this
  def determinant: Double = 1.0

  @targetName("m_add")
  override def +(other: Matrix): SquareMatrix = super.+(other).toSquareMatrix

  @targetName("m_subtract")
  override def -(other: Matrix): SquareMatrix = super.-(other).toSquareMatrix

  @targetName("m_scale")
  override def *(scalar: Double): SquareMatrix = super.*(scalar).toSquareMatrix

  @targetName("msquare_product")
  def *(other: SquareMatrix): Matrix = super.*(other).toSquareMatrix

  override def transpose: SquareMatrix = super.transpose.toSquareMatrix

  override def getSubMatrix(n: Int, m: Int): SquareMatrix = super.getSubMatrix(n, m).toSquareMatrix
}
