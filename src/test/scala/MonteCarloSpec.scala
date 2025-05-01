import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class GetDiceRollSpec extends AnyFlatSpec  with Matchers {
  "estimatingPi" should "return a value close to pi" in {
    MonteCarloEstimator.estPi(1000000) should equal (3.14 +- .1)
    MonteCarloEstimator.parEstPi(1000000) should equal (3.14 +- .1)
  }

  "estimatingE" should "return a value close to e" in {
    MonteCarloEstimator.estE(10000000) should equal(2.7 +- .1)
    MonteCarloEstimator.parEstE(10000000) should equal(2.7 +- .1)
  }

  "estimateNHeadsProbInMFlips with 1 head " should "return a value close to .5" in {
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 1, 1) should equal(.5 +- .01)
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 2, 2) should equal(.25 +- .01)
    MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 2, 3) should equal(0)
  }

}
