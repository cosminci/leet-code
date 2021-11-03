package io.github.cosminci.leetcode._500

object _410_SplitArrayLargestSum:
  def main(args: Array[String]): Unit =
    println(splitArray(Array(7, 2, 5, 10, 8), 2))
    println(splitArray(Array(1, 2, 3, 4, 5), 2))
    println(splitArray(Array(1, 4, 4), 3))

  def splitArray(nums: Array[Int], m: Int): Int =
    def canSplit(sumLimit: Int) =
      nums
        .foldLeft((1, 0)) { case ((subarrayCount, currentSum), n) =>
          if currentSum + n <= sumLimit then (subarrayCount, currentSum + n)
          else (subarrayCount + 1, n)
        }
        ._1 <= m

    var (l, r) = (nums.max, nums.sum)
    while l < r do
      val mid = l + (r - l) / 2
      if canSplit(mid) then r = mid
      else l = mid + 1
    l
