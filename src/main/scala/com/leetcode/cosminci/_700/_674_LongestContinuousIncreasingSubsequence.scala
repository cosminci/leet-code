package com.leetcode.cosminci._700

object _674_LongestContinuousIncreasingSubsequence:
  def main(args: Array[String]): Unit =
    println(findLengthOfLCIS(Array(0, 1, 2, 2, 2, 2, 2, 3, 4, 5)))

  def findLengthOfLCIS(nums: Array[Int]): Int =
    var (longestLength, currLength, currIdx) = (1, 1, 1)

    while currIdx < nums.length do
      if nums(currIdx) > nums(currIdx - 1) then currLength += 1
      else if currLength > longestLength then
        longestLength = currLength
        currLength = 1
      currIdx += 1

    math.max(longestLength, currLength)
