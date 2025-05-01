import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovChainSpec extends AnyFlatSpec with Matchers {

  "MarkovChain" should "return a vector equal to" in {
    val matrixA: Matrix = Matrix(3, 3, List(List(1, 1, 0), List(0, 0, 0), List(0, 0, 1)))
    print(matrixA)
    print("\n")
    val vectorA: MyVector = MyVector(List(0.3, 0.2, 0.5))
    print(vectorA)
    print("\n")
    MarkovChain(vectorA, matrixA, 3).toList should equal(MyVector(List(0.5, 0.0, 0.5)).toList)
  }
}