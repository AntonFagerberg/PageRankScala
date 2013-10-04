import java.util.Scanner
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

object Main {
  def printMatrix(matrix: ArrayBuffer[ArrayBuffer[Double]]): Unit = {
    matrix.foreach { row =>
      print("|")
      row.foreach(value => print(s" ${value.formatted("%.2f")} |"))
      println()
    }
    println()
  }

  def printVector(vector: ArrayBuffer[Double]): Unit = {
    print("|")
    vector.foreach(value => print(s" ${value.formatted("%.2f")} |"))
    println()
    println()
  }

  def printSparseMatrix(matrix: ArrayBuffer[(Int, Int, Double)]): Unit = {
    matrix.foreach { case (rowIndex, columnIndex, value) =>
      println(s" [$rowIndex, $columnIndex]: ${value.formatted("%.2f")}")
    }
    println()
    println()
  }

  def buildAFromFile(fileName: String): ArrayBuffer[ArrayBuffer[Double]] = {
    val scanner = new Scanner(io.Source.fromFile(fileName).bufferedReader())
    val size = scanner.nextInt

    val A = ArrayBuffer.fill(size){
      ArrayBuffer.fill(size)(0d)
    }

    while (scanner.hasNextInt) {
      val (from, to) = scanner.nextInt -> scanner.nextInt
      A(from)(to) = A(from)(to) + 1d
    }

    A
  }

  def buildHFromA(A: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[ArrayBuffer[Double]] = {
    A.map { row =>
      val sum = row.sum

      if (sum > 0) {
        row.map { value =>
          value / sum
        }
      } else {
        row
      }
    }
  }

  def buildDFromA(A: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[Double] = {
    A.map { row =>
      if (row.sum > 0) {
        0d
      } else {
        1d / A.length
      }
    }
  }

  def buildSparseMatrixFromH(H: ArrayBuffer[ArrayBuffer[Double]]): ArrayBuffer[(Int, Int, Double)] = {
    for {
      (row, rowIndex) <- H.zipWithIndex
      (value, columnIndex) <- row.zipWithIndex
      if value > 0
    } yield {
      (rowIndex, columnIndex, value)
    }
  }

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
//    val A = buildAFromFile("data/three.txt")
//    val size = A.length
//    val H = buildHFromA(A)
//    val D = buildDFromA(A)
//    val sparseH = buildSparseMatrixFromH(H)

    val (size, h, d) = buildSparseFromFile("data/p2p-Gnutella08-mod.txt")
    val alpha = 0.85d
    val oneMinusAlphaDividedByN = (1d - alpha) / size
    var p = ArrayBuffer.fill(size)(1d / size)

    (0 until 1000).foreach { index =>
      // Part 1
      val alphaP = p.map(alpha *)
      val alphaPH = ArrayBuffer.fill(size)(0d)
      h.foreach { case ((row, column), value) =>
        alphaPH(column) += alphaP(row) * value
      }
//      sparseH.map { case (row, column, value) =>
//        alphaPH(column) += alphaP(row) * value
//      }



      // Part 2
//      val sumD = alphaP.zip(D).map { case (a, b) => a * b}.sum

      val sumD =
        d.map { case (row, value) =>
          alphaP(row) * value
        }.sum

      // Part 3
      val oneMinusAlphaDividedByNP = p.sum * oneMinusAlphaDividedByN

      p = alphaPH.map(oneMinusAlphaDividedByNP + sumD +)
    }

//    printMatrix(A)
//    printMatrix(H)
//    printSparseMatrix(sparseH)
//    printVector(D)
//    printVector(p)
    p.zipWithIndex.sorted.map { case (value, index) =>
      println(s"$index: $value")
    }
  }
}
