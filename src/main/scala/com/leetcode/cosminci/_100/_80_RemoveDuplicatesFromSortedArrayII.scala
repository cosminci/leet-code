package com.leetcode.cosminci._100

object _80_RemoveDuplicatesFromSortedArrayII:
  def main(args: Array[String]): Unit =
    println(removeDuplicates(Array(1, 1, 1, 2, 2, 3)))
    println(removeDuplicates(Array(0, 0, 1, 1, 1, 1, 2, 3, 3)))

  def removeDuplicates(nums: Array[Int]): Int =
    nums.foldLeft(0) { case (i, n) =>
      if i < 2 || n != nums(i - 2) then
        nums(i) = n
        i + 1
      else i
    }
