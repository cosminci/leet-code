package io.github.cosminci.leetcode._600

object _540_SingleElementInSortedArray:
  def main(args: Array[String]): Unit =
    println(singleNonDuplicate(Array(1, 1, 2, 3, 3, 4, 4, 8, 8)))
    println(singleNonDuplicate(Array(3, 3, 7, 7, 10, 11, 11)))

  def singleNonDuplicate(nums: Array[Int]): Int =
    var (left, right) = (0, nums.length - 1)

    while left < right do
      val mid = left + (right - left) / 2
      if nums(mid) != nums(mid ^ 1) then right = mid
      else left = mid + 1

    nums(left)
