import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class SquareMatrixSpec extends AnyFlatSpec with Matchers {

  val mat2x2 = new SquareMatrix(2, List(List(4, 7), List(2, 6)))
  val matSingular = new SquareMatrix(2, List(List(1, 2), List(2, 4)))
  val identity = new SquareMatrix(2, List(List(1.0, 0.0), List(0.0, 1.0)))

  "determinant" should "compute determinant correctly" in {
    mat2x2.determinant should equal(10.0)
  }

  "determinantParallel" should "compute determinant in parallel correctly" in {
    matSingular.determinantParallel should equal(0.0)
  }

  "isInvertible" should "return true for invertible matrices" in {
    mat2x2.isInvertible shouldBe true
  }

  "isInvertible" should "return false for singular matrices" in {
    matSingular.isInvertible shouldBe false
  }

  "isInvertibleParallel" should "return true for invertible matrices" in {
    mat2x2.isInvertibleParallel shouldBe true
  }

  "isInvertibleParallel" should "return false for singular matrices" in {
    matSingular.isInvertibleParallel shouldBe false
  }

  "+" should "add two square matrices correctly" in {
    val sum = mat2x2 + identity
    sum.data should equal(List(List(5.0, 7.0), List(2.0, 7.0)))
  }

  "-" should "subtract two square matrices correctly" in {
    val diff = mat2x2 - identity
    diff.data should equal(List(List(3.0, 7.0), List(2.0, 5.0)))
  }

  "*" should "scale the matrix correctly" in {
    val scaled = mat2x2 * 2.0
    scaled.data should equal(List(List(8.0, 14.0), List(4.0, 12.0)))
  }

  "*" should "multiply two square matrices correctly" in {
    val result = mat2x2 * identity
    result.data should equal(mat2x2.data)
  }

  "transpose" should "transpose the matrix" in {
    val trans = mat2x2.transpose
    trans.data should equal(List(List(4.0, 2.0), List(7.0, 6.0)))
  }

  "getSubMatrix" should "return correct submatrix" in {
    val sub = mat2x2.getSubMatrix(0, 0)
    sub.data should equal(List(List(6.0)))
  }

  "adjoint" should "compute adjoint correctly" in {
    val adj = mat2x2.adjoint
    adj.data should equal(List(List(6.0, -7.0), List(-2.0, 4.0)))
  }

  "adjointParallel" should "compute adjoint correctly in parallel" in {
    val adj = mat2x2.adjointParallel
    adj.data should equal(List(List(6.0, -7.0), List(-2.0, 4.0)))
  }

  "inverse" should "compute the inverse of the matrix" in {
    val inv = mat2x2.inverse
    inv.data.zip(List(List(0.6, -0.7), List(-0.2, 0.4))).foreach { case (computedRow, expectedRow) =>
      computedRow.zip(expectedRow).foreach { case (computed, expected) =>
        computed should be(expected +- 0.0000001)
      }
    }
  }

  "inverseParallel" should "compute the inverse of the matrix in parallel" in {
    val inv = mat2x2.inverseParallel
    inv.data.zip(List(List(0.6, -0.7), List(-0.2, 0.4))).foreach { case (computedRow, expectedRow) =>
      computedRow.zip(expectedRow).foreach { case (computed, expected) =>
        computed should be(expected +- 0.0000001)
      }
    }
  }

  "inverse" should "throw on singular matrix" in {
    an[UnsupportedOperationException] should be thrownBy matSingular.inverse
  }

  "inverseParallel" should "throw on singular matrix" in {
    an[UnsupportedOperationException] should be thrownBy matSingular.inverseParallel
  }
}
