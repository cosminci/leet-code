package io.github.cosminci.leetcode._1100

import scala.collection.mutable

object _1074_NumberOfSubmatricesThatSumToTarget:
  def main(args: Array[String]): Unit =
    println(
      numSubmatrixSumTarget(
        Array(
          Array(0, 1, 0),
          Array(1, 1, 1),
          Array(0, 1, 0)
        ),
        0
      )
    )

  private def numSubmatrixSumTarget(matrix: Array[Array[Int]], target: Int): Int =
    val (m, n) = (matrix.length, matrix.head.length)
    (0 until m).foreach { r =>
      (1 until n).foreach { c =>
        matrix(r)(c) += matrix(r)(c - 1)
      }
    }
    var totalCount = 0
    (0 until n).foreach { startCol =>
      (startCol until n).foreach { endCol =>
        val prefixSums = mutable.Map(0 -> 1)
        var rollingSum = 0
        (0 until m).foreach { row =>
          rollingSum += matrix(row)(endCol) - (if startCol > 0 then matrix(row)(startCol - 1) else 0)
          totalCount += prefixSums.getOrElse(rollingSum - target, 0)
          prefixSums.updateWith(rollingSum) {
            case None    => Some(1)
            case Some(c) => Some(c + 1)
          }
        }
      }
    }
    totalCount
