import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.util.Random

object MonteCarloEstimator {
  private def genPiSample : Int = {
    val x = Random.nextDouble()
    val y = Random.nextDouble()

    val d = (x * x) + (y * y)

    if d <= 1 then 1 else 0
  }

  def estPi(N: Integer): Double = {
    val inCircle : Int = (0 until N).map(_ => genPiSample).sum

    4.0 * (inCircle.toDouble)/N
  }

  def parEstPi(N: Integer): Double = {
    val inCircle: Int = (0 until N).par.map(_ => genPiSample).sum

    4.0 * (inCircle.toDouble) / N
  }

  @tailrec
  private def eTrial(sum : Double = 0 , count: Int = 0): Int = {
    if sum >= 1.0 then return count

    eTrial(sum + Random.nextDouble(), count+1)
  }

  def estE(N:Integer): Double = {
    (0 until N).map(_ => eTrial(0.0)).sum.toDouble / N
  }

  def parEstE(N:Integer) : Double = {
    (0 until N).par.map(_ => eTrial(0.0)).sum.toDouble / N
  }

  @tailrec
  def flips(N: Int, numFlips: Int, count : Int = 0): Int = {
    // We got N heads in a row
    if count == N then return 1
    // We ran out of flips and didn't get n heads
    if numFlips == 0 then return 0

    // Continue trying
    flips(N, numFlips-1, if Random.nextInt(2) == 0 then count+1 else 0)
  }


  /**
   * Function to see the probability of get n heads in a row with numFlips attempts
   * @param numTrials - the number of trials we run monte carlo for
   * @param numFlips - the number of flips
   * @param N - the number of heads
   * @return - probability
   */
  def estimateNHeadsProbInMFlips(numTrials : Int, numFlips: Int, N: Int): Double = {
    (0 until numTrials).map(_ => flips(N, numFlips)).sum.toDouble/numTrials
  }

  def parEstimateNHeadsProbInMFlips(numTrials: Int, numFlips: Int, N: Int): Double = {
    if numFlips < N then return 0.0
    (0 until numTrials).par.map(_ => flips(N, numFlips)).sum.toDouble / numTrials
  }
}