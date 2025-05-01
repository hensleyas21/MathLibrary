import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MarkovChainSpec extends AnyFlatSpec with Matchers {

  "MarkovChain" should "return a vector equal to" in {
    val matrixA: Matrix = Matrix(3, 3, List(List(0.2, 0.9, 0.5), List(0.7, 0.0, 0.25), List(0.1, 0.1, 0.25)))
    print(matrixA)
    print("\n")
    val vectorA: MyVector = MyVector(List(0.3, 0.2, 0.5))
    val vectorB: MyVector = MyVector(List(0.5086, 0.372463, 0.118938))
    print(vectorA)
    print("\n")
    print(vectorB)
    print("\n")
    val listA: List[Double] = MarkovChain(vectorA, matrixA, 3).toList
    val listB: List[Double] = vectorB.data
    val vectorsAreEqual: List[Boolean] = (for pair <- listA.zip(listB) yield if pair._1 - pair._2 > 0.0001 then false else true)
    !vectorsAreEqual.contains(false)
  }
}