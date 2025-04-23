import scala.collection.immutable.Vector

@main
def main(): Unit = {
    val a = new Matrix(2, 3, List(List(1, 2, 3), List(4, 5, 6)))
    val b = new Matrix(3, 2, List(List(3, 1), List(4, 1), List(5, 9)))
    val c = new SquareMatrix(3, List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
    val d = new SquareMatrix(2, List(List(1, 2), List(0, 2)))
    print(d)
    println(d.determinant)
    val inv = d.inverse
    print(inv)
    println(inv.determinant)
    val temp = (inv * d).toSquareMatrix
    print(temp)
    println(temp.determinant)

    val v1 = new MyVector(List(-1, 2, 0.837))
    val v2 = new MyVector(List(3, 4, 5))
    print(v1.crossProduct(List(v2)).normalize)

}