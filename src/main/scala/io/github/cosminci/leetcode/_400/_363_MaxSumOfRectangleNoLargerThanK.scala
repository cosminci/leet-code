package io.github.cosminci.leetcode._400

import scala.collection.immutable.TreeSet

object _363_MaxSumOfRectangleNoLargerThanK:

  def maxSumSubmatrix(matrix: Array[Array[Int]], k: Int): Int =
    matrix.head.indices.foldLeft(Int.MinValue) { case (globalMax, lCol) =>
      (lCol until matrix.head.length)
        .foldLeft(globalMax, Seq.fill(matrix.length)(0)) { case ((globalMax, runningSums), rCol) =>
          val newRunningSums = runningSums.indices.map(row => runningSums(row) + matrix(row)(rCol))
          val prefixSum      = newRunningSums.scanLeft(0)(_ + _).tail

          val newGlobalMax = prefixSum.foldLeft(globalMax, TreeSet.empty[Int]) {
            case ((globalMax, prevSums), sum) =>
              val entireSubarraySum  = Option.when(sum <= k)(sum)
              val partialSubarraySum = prevSums.minAfter(sum - k).map(sum - _)
              val localMax           = (Array(globalMax) ++ entireSubarraySum ++ partialSubarraySum).max

              if (localMax == k) return localMax
              (localMax, prevSums + sum)
            }._1
          (newGlobalMax, newRunningSums)
        }._1
    }
