import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyVectorSpec extends AnyFlatSpec with Matchers {

  val v1: MyVector = MyVector(List(1, -2, 3))
  val v2: MyVector = MyVector(List(5, 0, -3))

  "size" should "return the size of the vector (3)" in {
    v1.size should equal (3)
  }

  "magnitude" should "return the magnitude of the vector" in {
    v1.magnitude should equal(Math.sqrt(14) +- .0000001)
    v2.magnitude should equal(Math.sqrt(34) +- .0000001)
  }

  "+" should "add two vectors element-wise" in {
    (v1 + v2).toList should equal(List(6, -2, 0))
  }

  "-" should "subtract two vectors element-wise" in {
    (v1 - v2).toList should equal(List(-4, -2, 6))
  }

  "*" should "scale the vector by a scalar" in {
    (v1 * 2.0).toList should equal(List(2.0, -4.0, 6.0))
  }

  "L1Norm" should "return the sum of absolute values" in {
    v1.L1Norm should equal(6.0)
    v2.L1Norm should equal(8.0)
  }

  "L2Norm" should "return the Euclidean norm" in {
    v1.L2Norm should equal(Math.sqrt(14) +- 0.000001)
  }

  "maximumNorm" should "return the max absolute value" in {
    v1.maximumNorm should equal(3.0)
    v2.maximumNorm should equal(5.0)
  }

  "normalize" should "return a unit vector in the same direction" in {
    val normalized = v1.normalize
    normalized.magnitude should equal(1.0 +- 0.000001)
    normalized.toList.zip(v1.toList).foreach { case (n, original) =>
      n should equal(original / Math.sqrt(14) +- 0.000001)
    }
  }

  "dotProduct" should "return the dot product of two vectors" in {
    v1.dotProduct(v2) should equal(-4.0)
  }

  "crossProduct" should "compute the cross product of two 3D vectors" in {
    val result = v1.crossProduct(List(v2))
    result.toList.map(x => x +- 0.000001) should equal(List(6.0, 18.0, 10.0).map(_ +- 0.000001))
  }

  "toString" should "format the vector nicely" in {
    v1.toString should equal("<1.0, -2.0, 3.0>")
  }

  "toList" should "return the internal list representation" in {
    v1.toList should equal(List(1.0, -2.0, 3.0))
  }

  "operations" should "throw exception if vector sizes mismatch" in {
    val vShort = MyVector(List(1.0, 2.0))
    an[UnsupportedOperationException] should be thrownBy (v1 + vShort)
    an[UnsupportedOperationException] should be thrownBy (v1 - vShort)
    an[UnsupportedOperationException] should be thrownBy (v1.dotProduct(vShort))
  }

  "crossProduct" should "throw exception if not enough vectors are provided" in {
    an[UnsupportedOperationException] should be thrownBy (v1.crossProduct(List()))
  }

}
