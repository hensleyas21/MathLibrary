import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class FunctionsSpec extends AnyFlatSpec with Matchers {

  "MarkovChain" should "return a vector equal to" in {
    val matrixA: Matrix = Matrix(3, 3, List(List(1, 1, 0), List(0, 0, 0), List(0, 0, 1)))
    print(matrixA)
    print("\n")
    val vectorA: MyVector = MyVector(List(0.3, 0.2, 0.5))
    print(vectorA)
    print("\n")
    MarkovChain(vectorA, matrixA, 3).toList should equal(MyVector(List(0.5, 0.0, 0.5)).toList)
  }

  "estimatingE" should "return a value close to e" in {
    MonteCarloEstimator.estE(10000000) should equal(2.7 +- .1)
    MonteCarloEstimator.parEstE(10000000) should equal(2.7 +- .1)
  }

  "estimateNHeadsProbInMFlips with 1 head " should "return a value close to .5" in {
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 1, 1) should equal(.5 +- .01)
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 2, 2) should equal(.25 +- .01)
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 2, 3) should equal(0)


    MonteCarloEstimator.parEstimateNHeadsProbInMFlips(100000000, 1, 1) should equal(.5 +- .01)
    MonteCarloEstimator.parEstimateNHeadsProbInMFlips(100000000, 2, 2) should equal(.25 +- .01)
    MonteCarloEstimator.parEstimateNHeadsProbInMFlips(100000000, 2, 3) should equal(0)
  }
}