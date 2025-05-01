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
        timeE()
    timeFlips()
}

def timePi() : Unit = {
    print("============\nTiming Pi\n============\n")
    var result = timeIt(MonteCarloEstimator.estPi(1000000))
    print(s"Estimating pi not in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstPi(1000000))
    print(s"Estimating pi in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estPi(100000000))
    print(s"\nEstimating pi not in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstPi(100000000))
    print(s"Estimating pi in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estPi(500000000))
    print(s"\nEstimating pi not in parallel took [${result._1} ms] for 500 million trials ")
    println(s"Pi is ${result._2}")

    //    result = timeIt(MonteCarloEstimator.parEstPi(500000000))
    print(s"Estimating pi in parallel took [infinite ms] for 500 million trials ")
    println("We ran out of heap memory")
}


def timeFlips(): Unit = {
    print("============\nTiming Flips\n============\n")
    var result = timeIt(MonteCarloEstimator.estimateNHeadsProbInMFlips(500000, 100, 5))
    print(s"\nEstimating flips with 100 flips to get 5 heads in a row not in parallel took [${result._1} ms] for 500 thousand trials ")
    println(s"Prob is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstimateNHeadsProbInMFlips(500000, 100, 5))
    print(s"Estimating flips with 100 flips to get 10 heads in a row not in parallel took [${result._1} ms] for 500 thousand trials ")
    println(s"Prob is ${result._2}")

    result = timeIt(MonteCarloEstimator.estimateNHeadsProbInMFlips(1000000, 100, 10))
    print(s"\nEstimating flips with 100 flips to get 10 heads in a row not in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Prob is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstimateNHeadsProbInMFlips(1000000, 100, 10))
    print(s"Estimating flips with 100 flips to get 10 heads in a row not in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Prob is ${result._2}")

    result = timeIt(MonteCarloEstimator.estimateNHeadsProbInMFlips(100000000, 150, 25))
    print(s"\nEstimating flips with 150 flips to get 25 heads in a row not in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Prob is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstimateNHeadsProbInMFlips(  100000000, 150, 25))
    print(s"Estimating flips with 150 flips to get 25 heads in a row not in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Prob is ${result._2}")
}

def timeE() : Unit = {
    print("\n\n============\nTiming E\n============\n")
    var result = timeIt(MonteCarloEstimator.estE(1000000))
    print(s"Estimating e not in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstE(1000000))
    print(s"Estimating e in parallel took [${result._1} ms] for 1 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estE(100000000))
    print(s"\nEstimating e not in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.parEstE(100000000))
    print(s"Estimating e in parallel took [${result._1} ms] for 100 million trials ")
    println(s"Pi is ${result._2}")

    result = timeIt(MonteCarloEstimator.estE(500000000))
    print(s"\nEstimating e not in parallel took [${result._1} ms] for 500 million trials ")
    println(s"Pi is ${result._2}")

    //    result = timeIt(MonteCarloEstimator.parEstE(500000000))
    print(s"Estimating e in parallel took [infinite ms] for 500 million trials ")
    println("We ran out of memory")
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