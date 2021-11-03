package io.github.cosminci.leetcode._1600

object _1539_KthMissingPositiveNumber:
  def main(args: Array[String]): Unit =
    println(findKthPositive(Array(2, 3, 4, 7, 11), 5))
    println(findKthPositive(Array(1, 2, 3, 4), 2))

  def findKthPositive(arr: Array[Int], k: Int): Int =
    var (l, r) = (0, arr.length)

    while l < r do
      val mid = l + (r - l) / 2
      if arr(mid) - (mid + 1) < k then l = mid + 1
      else r = mid

    l + k
