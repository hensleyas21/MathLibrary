import scala.collection.immutable.Vector

@main
def main(): Unit = {
    timeMonteCarlo()
    timeRiemannSum()
}

def timeIt[A](f: => A): (Double, A) = {
    val startTime = System.currentTimeMillis()
    val result = f
    val endTime = System.currentTimeMillis()
    (endTime-startTime, result)
}

def timeMonteCarlo() : Unit = {
    timePi()
}

def timePi() : Unit = {
    var result = timeIt(MonteCarloEstimator.estPi(1000000))
    print(s"Estimating pi not in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstPi(1000000))
    print(s"Estimating pi in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estPi(50000000))
    print(s"Estimating pi not in parallel took [${result._1} ms] for 50 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstPi(50000000))
    print(s"Estimating pi in parallel took [${result._1} ms] for 50 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estPi(100000000))
    print(s"Estimating pi not in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstPi(100000000))
    print(s"Estimating pi in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")
}

def timeRiemannSum(): Unit ={
    def squared(x: Double): Double = x*x

    val subintervals = 100000000
    val start = 0
    val end = 10
    print(s"For testing we are calculating the Riemann sum for the function x*x from ${start} to ${end} with ${subintervals} subintervals:")
    println("")
    val (time1,lrs) = timeIt(leftRiemannSum(squared,start,end,subintervals))
    println(s"Calculating left Riemann Sum took ${time1} ms for the given integral")
    println(s"The left Riemann Sum is ${lrs}")

    val (time2, lrsp) = timeIt(leftRiemannSumParallel(squared,start,end,subintervals))
    println(s"Calculating left Riemann Sum in parallel took ${time2} ms for the given integral")
    println(s"The left Riemann Sum is ${lrsp}")

    val (time3, rrs) = timeIt(rightRiemannSum(squared,start,end,subintervals))
    println(s"Calculating right Riemann Sum took ${time3} ms for the given integral")
    println(s"The right Riemann Sum is ${rrs}")

    val (time4, rrsp) = timeIt(rightRiemannSumParallel(squared,start,end,subintervals))
    println(s"Calculating right Riemann Sum in parallel took ${time4} ms for the given integral")
    println(s"The right Riemann Sum is ${rrsp}")

    val (time5, mrs) = timeIt(middleRiemannSum(squared,start,end,subintervals))
    println(s"Calculating middle Riemann Sum took ${time5} ms for the given integral")
    println(s"The middle Riemann Sum is ${mrs}")

    val (time6, mrsp) = timeIt(middleRiemannSumParallel(squared,start,end,subintervals))
    println(s"Calculating middle Riemann Sum in parallel took ${time6} ms for the given integral")
    println(s"The middle Riemann Sum is ${mrsp}")


}