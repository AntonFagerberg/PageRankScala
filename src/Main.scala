import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Main {
  def printMap[K, V](map: mutable.HashMap[(K, K), V]) {
    map.map { case ((row, column), value) =>
      println(s"[$row, $column] = ${value.toString.toFloat.formatted("%.2f")}")
    }
    println()
  }

  def buildSparseFromFile(fileName: String): (Int, mutable.HashMap[(Int, Int), Double], mutable.HashMap[Int, Double]) = {
    val scanner = new Scanner(io.Source.fromFile(fileName).bufferedReader())
    val size = scanner.nextInt
    val A = new mutable.HashMap[(Int, Int), Int]
    val rowCount = new mutable.HashMap[Int, Int]

    while (scanner.hasNextInt) {
      val (row, column) = scanner.nextInt -> scanner.nextInt
      rowCount(row) = rowCount.get(row).getOrElse(0) + 1
      A(row -> column) = A.get(row -> column).getOrElse(0) + 1
    }

    val H = A.map { case ((row, column), value) =>
      (row -> column) -> value.toDouble / rowCount(row)
    }

    val D = new mutable.HashMap[Int, Double]
    (0 until size).foreach { row =>
      if (!rowCount.contains(row)) {
        D(row) = 1d / size
      }
    }

    (size, H, D)
  }

  def main(args: Array[String]): Unit = {
    val (size, h, d) = buildSparseFromFile("data/p2p-Gnutella08-mod.txt")
    val alpha = 0.85d
    val oneMinusAlphaDividedByN = (1d - alpha) / size
    var p = ArrayBuffer.fill(size)(1d / size)
    val alphaPH = ArrayBuffer.fill(size)(0d)
    var sumP = 0d

    (0 until 1000).foreach { index =>
      sumP = p.sum
      p.transform(alpha *)

      alphaPH.transform(i => 0d)
      h.foreach { case ((row, column), value) =>
        alphaPH(column) += p(row) * value
      }

      p = alphaPH.map(sumP * oneMinusAlphaDividedByN + d.map { case (row, value) => p(row) * value }.sum +)
    }

    p.zipWithIndex.sorted.map { case (value, index) =>
      println(s"$index: $value")
    }
  }
}