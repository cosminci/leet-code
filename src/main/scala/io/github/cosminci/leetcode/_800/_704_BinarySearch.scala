package io.github.cosminci.leetcode._800

object _704_BinarySearch:
  def main(args: Array[String]): Unit =
    println(search(Array(-1, 0, 3, 5, 9, 12), 9))
    println(search(Array(-1, 0, 3, 5, 9, 12), 2))

  private def search(nums: Array[Int], target: Int): Int =
    var (l, r) = (0, nums.length - 1)
    while l < r do
      val mid = l + (r - l) / 2
      if nums(mid) < target then l = mid + 1
      else r = mid
    if nums(l) == target then l else -1
