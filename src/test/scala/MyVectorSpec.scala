import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MyVectorSpec extends AnyFlatSpec with Matchers {

  "countNucleotides" should "count single occurrences" in {
    Homework2.countNucleotides("ACGT") should equal (Map('A' -> 1, 'C' -> 1, 'G' -> 1, 'T' -> 1))
  }
  
}
