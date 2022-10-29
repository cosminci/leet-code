package com.leetcode.cosminci._600

import scala.collection.mutable

object _565_ArrayNesting:
  def main(args: Array[String]): Unit =
    println(arrayNesting(Array(5, 4, 0, 3, 1, 6, 2)))

  def arrayNesting(nums: Array[Int]): Int =
    var maxCycleLength = 0
    nums.indices.foreach { startIdx =>
      var (currCycleLength, currIdx) = (0, startIdx)
      while currIdx >= 0 && nums(currIdx) >= 0 do
        val nextIdx = nums(currIdx)
        nums(currIdx) = -1
        currIdx = nextIdx
        currCycleLength += 1
      maxCycleLength = math.max(maxCycleLength, currCycleLength)
    }
    maxCycleLength
