package io.github.cosminci.leetcode._100

object _33_SearchInRotatedSortedArray:
  def main(args: Array[String]): Unit =
    println(search(Array(5, 1, 3), 2))
    println(search(Array(4, 5, 6, 7, 0, 1, 2), 2))

  def search(nums: Array[Int], target: Int): Int = {
    if (nums.length < 2) return nums.indexOf(target)

    var (l, r) = (0, nums.length)

    while (l < r) {
      val mid = l + (r - l) / 2

      val midValue =
        if (nums(mid) < nums(l) == target < nums(l)) nums(mid)
        else if (target < nums(l)) Int.MinValue
        else Int.MaxValue

      if (target < midValue) r = mid
      else if (target > midValue) l = mid + 1
      else return mid
    }

    -1
  }
