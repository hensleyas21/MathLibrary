import scala.util.Random

@main
def main(): Unit = {
    // comment these out as needed
    timeMonteCarlo()
    timeRiemannSum()
    timeMarkovChain()
    timeDeterminants()
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

def timeMarkovChain(): Unit = {
    val matrix: Matrix = Matrix(5, 5, List(
        List(0.5, 0.1, 0.3, 0.2, 0),
        List(0.25, 0.6, 0.5, 0.7, 0.3),
        List(0.15, 0.2, 0.2, 0.1, 0.5),
        List(0.1, 0.0, 0.0, 0.0, 0.1),
        List(0.0, 0.1, 0.0, 0.0, 0.1)))
    val vector: MyVector = MyVector(List(0.2, 0.3, 0.1, 0.1, 0.3))

    val seqResult1 = timeIt(MarkovChain(vector, matrix, 1000000))
    val parResult1 = timeIt(ParMarkovChain(vector, matrix, 1000000))
    println(s"The Markov chain for a 5 x 5 matrix, computed sequentially with 1,000,000 iterations took [${seqResult1._1} ms]")
    println(s"The result was the vector ${seqResult1._2}")
    println(s"The Markov chain for a 5 x 5 matrix, computed in parallel with 1,000,000 iterations took [${parResult1._1} ms]")
    println(s"The result was the vector ${parResult1._2}")

    val seqResult2 = timeIt(MarkovChain(vector, matrix, 10000000))
    val parResult2 = timeIt(ParMarkovChain(vector, matrix, 10000000))
    println(s"The Markov chain for a 5 x 5 matrix, computed sequentially with 5,000,000 iterations took [${seqResult2._1} ms]")
    println(s"The result was the vector ${seqResult2._2}")
    println(s"The Markov chain for a 5 x 5 matrix, computed in parallel with 5,000,000 iterations took [${parResult2._1} ms]")
    println(s"The result was the vector ${parResult2._2}")

    val seqResult3 = timeIt(MarkovChain(vector, matrix, 50000000))
    val parResult3 = timeIt(ParMarkovChain(vector, matrix, 50000000))
    println(s"The Markov chain for a 5 x 5 matrix, computed sequentially with 10,000,000 iterations took [${seqResult3._1} ms]")
    println(s"The result was the vector ${seqResult3._2}")
    println(s"The Markov chain for a 5 x 5 matrix, computed in parallel with 10,000,000 iterations took [${parResult3._1} ms]")
    println(s"The result was the vector ${parResult3._2}")
}

def timeDeterminants(): Unit = {
    def generateRandomSquareMatrix(n: Int, min: Double = -10.0, max: Double = 10.0): SquareMatrix = {
        val random = new Random
        val data = (0 until n).map { _ =>
            (0 until n).map { _ =>
                min + (max - min) * random.nextDouble()
            }.toList
        }.toList
        new SquareMatrix(n, data)
    }

    for n <- 1 to 11 do {
        val m = generateRandomSquareMatrix(n)
        val result = timeIt(m.determinant)
        val resultPar = timeIt(m.determinantParallel)
        println(s"Det of ${n}x${n} matrix took [${result._1} ms]")
        println(s"Par det of ${n}x${n} matrix took [${resultPar._1} ms]\n")
    }

}