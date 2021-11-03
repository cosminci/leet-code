package io.github.cosminci.leetcode._2000

import scala.collection.mutable

object _1959_MinTotalSpaceWastedWithKResizingOps:
  def main(args: Array[String]): Unit =
    println(minSpaceWastedKResizing(Array(10, 20, 15, 30, 20), 2))

  def minSpaceWastedKResizing(nums: Array[Int], k: Int): Int =
    val n = nums.length
    if k == n then return 0

    val dp = Array.fill(n, k + 1)(Int.MaxValue)
    (0 until n).foreach { idx =>
      (0 to k).foreach { numCuts =>
        var (max, sum) = (0, 0)
        (idx to 0 by -1).foreach { cutIdx =>
          sum += nums(cutIdx)
          max = math.max(max, nums(cutIdx))

          val wasted = max * (idx - cutIdx + 1) - sum
          dp(idx)(numCuts) =
            if idx == 0 || numCuts == 0 then wasted
            else if cutIdx == 0 then math.min(dp(idx)(numCuts), wasted)
            else math.min(dp(idx)(numCuts), wasted + dp(cutIdx - 1)(numCuts - 1))
        }
      }
    }
    dp(n - 1)(k)
