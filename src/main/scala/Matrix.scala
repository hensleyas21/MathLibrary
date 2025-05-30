import scala.annotation.targetName

class Matrix(val n: Int, val m: Int, val data: List[List[Double]]) {

  def size: (Int, Int) = (n, m)

  def this(vectorList: List[MyVector]) = this(vectorList.length, vectorList.head.size, vectorList.map(v => v.toList))

  def isSquare: Boolean = n == m

  def getValue(n: Int, m: Int): Double = {
    if n < 0 || m < 0 || n >= this.n || m >= this.m then throw new IndexOutOfBoundsException("Invalid indices for matrix") else
    data(n)(m)
  }

  def getRow(i: Int): MyVector = {
    if i < 0 || i >= n then throw new IndexOutOfBoundsException("Invalid row index for matrix") else
    new MyVector(data(i))
  }

  def getColumn(i: Int): MyVector = {
    if i < 0 || i >= m then throw new IndexOutOfBoundsException("Invalid row index for matrix") else
    new MyVector(data.map(row => row(i)))
  }

  def transpose: Matrix = new Matrix(m, n, data.transpose)

  @targetName("m_add")
  def +(other: Matrix): Matrix = {
    if size != other.size then throw new UnsupportedOperationException("Matrices must be the same size") else
    new Matrix(n, m, data.zip(other.data).map({case (r1, r2) => r1.zip(r2).map({case (a, b) => a + b })}))
  }

  @targetName("m_subtract")
  def -(other: Matrix): Matrix = {
    if size != other.size then throw new UnsupportedOperationException("Matrices must be the same size") else
    new Matrix(n, m, data.zip(other.data).map({ case (r1, r2) => r1.zip(r2).map({ case (a, b) => a - b }) }))
  }

  @targetName("m_scale")
  def *(scalar: Double): Matrix = {
    new Matrix(n, m, data.map(row => row.map(x => x * scalar)))
  }

  @targetName("m_product")
  def *(other: Matrix): Matrix = {
    if m != other.n then throw new UnsupportedOperationException("The matrices' dimensions do not match") else
    new Matrix(n, other.m, (for x <- 0 until n yield (for y <- 0 until other.m yield getRow(x).dotProduct(other.getColumn(y))).toList).toList)
  }

  def getSubMatrix(n: Int, m: Int): Matrix = {
    if n < 0 || m < 0 || n >= this.n || m >= this.m then throw new IndexOutOfBoundsException("Invalid indices for matrix") else
    new Matrix(this.n-1, this.m-1, (for row <- 0 until this.n if row != n yield (for col <- 0 until this.m if col != m yield data(row)(col)).toList).toList)
  }

  def toSquareMatrix: SquareMatrix = if !isSquare then throw new UnsupportedOperationException("The matrix is not a square") else new SquareMatrix(n, data)

  override def toString: String = (for row <- data yield row.mkString("| ", "  ", " |\n")).mkString
}
