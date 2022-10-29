package com.leetcode.cosminci._100

object _26_RemoveDuplicatesFromSortedArray:

  def main(args: Array[String]): Unit =
    println(removeDuplicates(Array(0, 0, 1, 1, 1, 2, 2, 3, 3, 4)))

  def removeDuplicates(nums: Array[Int]): Int =
    if nums.isEmpty then return 0

    var i = 0
    var j = 0
    while j < nums.length do
      if nums(i) != nums(j) then
        i += 1
        nums(i) = nums(j)
      j += 1
    i + 1
