package io.github.cosminci.leetcode._200

object _154_FindMinInRotatedSortedArrayII:
  def main(args: Array[String]): Unit =
    println(findMin(Array(1, 3, 5)))
    println(findMin(Array(2, 2, 2, 0, 1)))

  private def findMin(nums: Array[Int]): Int =
    var (l, r) = (0, nums.length - 1)

    while l < r do
      val mid = l + (r - l) / 2

      if nums(mid) > nums(r) then l = mid + 1
      else if nums(mid) < nums(l) then r = mid
      else r -= 1

    nums(l)
