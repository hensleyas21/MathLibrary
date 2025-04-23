
@main
def main(): Unit = {
  val a = new Matrix(2, 3, List(List(1, 2, 3), List(4, 5, 6)))
  val b = new Matrix(3, 2, List(List(3, 1), List(4, 1), List(5, 9)))
  print(b * a)
}

