package io.github.cosminci.leetcode._600

object _523_ContinuousSubarraySum:
  def main(args: Array[String]): Unit =
    println(checkSubarraySum(Array(23, 2, 4, 6, 6), 7))
    println(checkSubarraySum(Array(1, 0), 2))
    println(checkSubarraySum(Array(23, 2, 4, 6, 7), 6))
    println(checkSubarraySum(Array(23, 2, 6, 4, 7), 6))
    println(checkSubarraySum(Array(23, 2, 6, 4, 7), 13))

  private def checkSubarraySum(nums: Array[Int], k: Int): Boolean =
    nums.indices.foldLeft(Map(0 -> -1), 0) {
      case ((prevSums, runningMod), i) =>
        val newRunningMod = (runningMod + nums(i)) % k
        if prevSums.get(newRunningMod).exists(j => i - j > 1) then return true

        val newPrevSums =
          if prevSums.contains(newRunningMod) then prevSums
          else prevSums.updated(newRunningMod, i)

        (newPrevSums, newRunningMod)
    }
    false
