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

    (0 until 100).foreach { index =>
      // Part 1
      val alphaP = p.map(alpha *)
      val alphaPH = ArrayBuffer.fill(size)(0d)
      h.foreach { case ((row, column), value) =>
        alphaPH(column) += alphaP(row) * value
      }

      // Part 2
      val sumD =
        d.map { case (row, value) =>
          alphaP(row) * value
        }.sum

      // Part 3
      val oneMinusAlphaDividedByNP = p.sum * oneMinusAlphaDividedByN

      p = alphaPH.map(oneMinusAlphaDividedByNP + sumD +)
    }

    p.zipWithIndex.sorted.reverse.map { case (value, index) =>
      println(s"$index: $value")
    }
  }
}