import scala.collection.immutable.Vector

@main
def main(): Unit = {
    timeMonteCarlo()
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