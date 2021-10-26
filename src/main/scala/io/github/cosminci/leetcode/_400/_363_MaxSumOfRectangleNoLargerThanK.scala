package io.github.cosminci.leetcode._400

object _363_MaxSumOfRectangleNoLargerThanK:
  def main(args: Array[String]): Unit =
    println(maxSumSubmatrix(Array(Array(5, -4, -3, 4), Array(-3, -4, 4, 5), Array(5, 1, 5, -4)), 10))
    println(maxSumSubmatrix(Array(Array(1, 0, 1), Array(0, -2, 3)), 2))
    println(maxSumSubmatrix(Array(Array(2, 2, -1)), 3))
    println(maxSumSubmatrix(Array(Array(2, 2, -1)), 0))

  private def maxSumSubmatrix(matrix: Array[Array[Int]], k: Int): Int =
    var globalMax = Int.MinValue

    matrix.head.indices.foreach { left =>
      val runningSums = Array.ofDim[Int](matrix.length)

      (left until matrix.head.length).foreach { right =>
        val sumTracker = new java.util.TreeSet[Int]
        matrix.indices.foreach(idx => runningSums(idx) += matrix(idx)(right))

        var maxSubarraySum = 0
        runningSums.indices.foreach { idx =>
          maxSubarraySum += runningSums(idx)
          if maxSubarraySum == k then return k
          if maxSubarraySum <= k && maxSubarraySum > globalMax then globalMax = maxSubarraySum

          Option(sumTracker.ceiling(maxSubarraySum - k)).foreach { subtractable =>
            val localMax = maxSubarraySum - subtractable
            if localMax == k then return k
            if localMax > globalMax then globalMax = localMax
          }
          sumTracker.add(maxSubarraySum)
        }
      }
    }
    globalMax
