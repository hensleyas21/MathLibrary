import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MatrixSpec extends AnyFlatSpec with Matchers {

  val matA = new Matrix(2, 3, List(List(1, 2, 3), List(4, 5, 6)))
  val matB = new Matrix(2, 3, List(List(7, 8, 9), List(10, 11, 12)))
  val squareMat = new Matrix(2, 2, List(List(1, 2), List(3, 4)))

  "size" should "return the correct matrix dimensions" in {
    matA.size shouldEqual (2, 3)
  }

  "isSquare" should "identify square and non-square matrices" in {
    (matA.isSquare, squareMat.isSquare) shouldBe (false, true)
  }

  "getValue" should "return the value at a given position" in {
    matA.getValue(1, 2) shouldEqual 6.0
  }

  it should "throw IndexOutOfBoundsException for invalid indices" in {
    an[IndexOutOfBoundsException] should be thrownBy matA.getValue(2, 0)
    an[IndexOutOfBoundsException] should be thrownBy matA.getValue(0, 3)
  }

  "getRow" should "return a MyVector representing the row" in {
    matA.getRow(0).toList shouldEqual List(1, 2, 3)
  }

  it should "throw IndexOutOfBoundsException for invalid row index" in {
    an[IndexOutOfBoundsException] should be thrownBy matA.getRow(2)
  }

  "getColumn" should "return a MyVector representing the column" in {
    matA.getColumn(1).toList shouldEqual List(2, 5)
  }

  it should "throw IndexOutOfBoundsException for invalid column index" in {
    an[IndexOutOfBoundsException] should be thrownBy matA.getColumn(3)
  }

  "transpose" should "transpose the matrix correctly" in {
    val transposed = matA.transpose
    transposed.getRow(1).toList shouldEqual List(2, 5)
  }

  "+" should "add two matrices element-wise" in {
    val result = matA + matB
    result.data shouldEqual List(List(8, 10, 12), List(14, 16, 18))
  }

  it should "throw if matrices are not the same size" in {
    an[UnsupportedOperationException] should be thrownBy (matA + squareMat)
  }

  "-" should "subtract two matrices element-wise" in {
    val result = matB - matA
    result.data shouldEqual List(List(6, 6, 6), List(6, 6, 6))
  }

  it should "throw if matrices are not the same size" in {
    an[UnsupportedOperationException] should be thrownBy (matA - squareMat)
  }

  "*" should "scale the matrix by a scalar" in {
    val result = matA * 2.0
    result.data shouldEqual List(List(2.0, 4.0, 6.0), List(8.0, 10.0, 12.0))
  }

  it should "multiply two matrices correctly" in {
    val matC = new Matrix(3, 2, List(List(1, 2), List(3, 4), List(5, 6)))
    val result = matA * matC
    result.data shouldEqual List(
      List(1*1 + 2*3 + 3*5, 1*2 + 2*4 + 3*6),
      List(4*1 + 5*3 + 6*5, 4*2 + 5*4 + 6*6)
    )
  }

  it should "throw if matrix dimensions are incompatible for multiplication" in {
    an[UnsupportedOperationException] should be thrownBy (matA * squareMat)
  }

  "getSubMatrix" should "remove the specified row and column" in {
    val mat = new Matrix(3, 3, List(
      List(1, 2, 3),
      List(4, 5, 6),
      List(7, 8, 9)
    ))
    val sub = mat.getSubMatrix(0, 1)
    sub.data shouldEqual List(
      List(4, 6),
      List(7, 9)
    )
  }

  it should "throw IndexOutOfBoundsException for invalid indices" in {
    an[IndexOutOfBoundsException] should be thrownBy squareMat.getSubMatrix(2, 0)
  }

  "toSquareMatrix" should "convert a square matrix" in {
    noException should be thrownBy squareMat.toSquareMatrix
  }

  it should "throw if matrix is not square" in {
    an[UnsupportedOperationException] should be thrownBy matA.toSquareMatrix
  }

  "toString" should "format the matrix properly" in {
    val expected = "| 1.0  2.0  3.0 |\n| 4.0  5.0  6.0 |\n"
    matA.toString shouldEqual expected
  }

}
