package io.github.cosminci.leetcode._1100

object _1043_PartitionArrayForMaxSum:

  def main(args: Array[String]): Unit =
    println(maxSumAfterPartitioningTopDown(Array(1, 4, 1, 5, 7, 3, 6, 1, 9, 9, 3), 4))
    println(maxSumAfterPartitioningBottomUp(Array(1, 4, 1, 5, 7, 3, 6, 1, 9, 9, 3), 4))

  def maxSumAfterPartitioningTopDown(arr: Array[Int], k: Int): Int =
    val mem = Array.fill[Int](arr.length)(Int.MinValue)

    def dfs(start: Int): Int =
      if start >= arr.length then return 0
      if mem(start) != Int.MinValue then return mem(start)

      val result = (1 to k)
        .collect {
          case numsToTake if start + numsToTake <= arr.length =>
            val taken = arr.slice(start, start + numsToTake)
            taken.max * numsToTake + dfs(start + numsToTake)
        }
        .maxOption
        .getOrElse(0)

      mem.update(start, result)
      result
    dfs(start = 0)

  def maxSumAfterPartitioningBottomUp(arr: Array[Int], k: Int): Int =
    val dp = Array.ofDim[Int](arr.length)
    arr.indices.foreach { i =>
      val value = (0 until k).map { numsToTakeIdx =>
        val numsToTake = numsToTakeIdx + 1
        Option
          .when(arr.isDefinedAt(i - numsToTakeIdx))(arr.slice(i - numsToTakeIdx, i + 1))
          .flatMap(_.maxOption)
          .getOrElse(0) * numsToTake +
          Option
            .when(dp.isDefinedAt(i - numsToTake))(dp(i - numsToTake))
            .getOrElse(0)
      }
      dp(i) = value.max
    }
    dp.max
