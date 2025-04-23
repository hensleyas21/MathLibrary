import scala.annotation.targetName

class Matrix(val n: Int, val m: Int, val data: List[List[Double]]) {

  def size: (Int, Int) = (n, m)

  def this(vectorList: List[Vector]) = this(vectorList.length, vectorList.head.size, vectorList.map(v => v.toList))

  def isSquare: Boolean = n == m

  def getValue(n: Int, m: Int): Double = {
    if n < 0 || m < 0 || n >= this.n || m >= this.m then throw new IndexOutOfBoundsException("Invalid indices for matrix") else
    data(n)(m)
  }

  def getRow(i: Int): Vector = {
    if i < 0 || i >= n then throw new IndexOutOfBoundsException("Invalid row index for matrix") else
    new Vector(data(i))
  }

  def getColumn(i: Int): Vector = {
    if i < 0 || i >= m then throw new IndexOutOfBoundsException("Invalid row index for matrix") else
    new Vector(data.map(row => row(i)))
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

  override def toString: String = (for row <- data yield row.mkString("| ", "  ", " |\n")).mkString
}
