import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RiemannSumSpec extends AnyFlatSpec with Matchers {

"leftRiemannSum" should "calculate the left Riemann Sum" in {
  def f(x:Double): Double ={
    x * x
  }
  leftRiemannSum(f,0,2,4) should equal (1.75 +- 0.01)
}

  "rightRiemannSum" should "calculate the right Riemann Sum" in {
    def f(x:Double): Double ={
      x * x
    }
    rightRiemannSum(f,0,2,4) should equal (3.75 +- 0.01)
  }

  "middleRiemannSum" should "calculate the middle Riemann Sum" in {
    def f(x:Double): Double ={
      x * x
    }
    middleRiemannSum(f,0,2,4) should equal (2.625 +- 0.01)
  }


  "leftRiemannSumParallel" should "calculate the left Riemann Sum in parallel" in {
    def f(x:Double): Double ={
      x * x
    }
    leftRiemannSumParallel(f,0,2,4) should equal (1.75 +- 0.01)
  }

  "rightRiemannSumParallel" should "calculate the right Riemann Sum in parallel" in {
    def f(x:Double): Double ={
      x * x
    }
    rightRiemannSumParallel(f,0,2,4) should equal (3.75 +- 0.01)
  }

  "middleRiemannSumParallel" should "calculate the middle Riemann Sum in parallel" in {
    def f(x:Double): Double ={
      x * x
    }
    middleRiemannSumParallel(f,0,2,4) should equal (2.625 +- 0.01)
  }

}