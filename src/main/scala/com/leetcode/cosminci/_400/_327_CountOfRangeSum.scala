package com.leetcode.cosminci._400

object _327_CountOfRangeSum:

  def main(args: Array[String]): Unit =
    print(countRangeSum(Array(5, 2, -2, 4, 1, 3, -2), 0, 4))

  def countRangeSum(nums: Array[Int], lower: Int, upper: Int): Int =
    val prefixSums = nums.scanLeft(0L)(_ + _)
    val temp       = Array.ofDim[Long](prefixSums.length)
    var count      = 0

    def mergeSort(start: Int, end: Int): Unit =
      if start >= end then return
      val mid = start + (end - start) / 2
      mergeSort(start, mid)
      mergeSort(mid + 1, end)
      merge(start, mid, end)

    def merge(start: Int, mid: Int, end: Int): Unit =
      var rightIdx              = mid + 1
      var (rightLow, rightHigh) = (mid + 1, mid + 1)
      var mergeIdx              = start
      (start to mid).foreach { leftIdx =>
        while rightLow <= end && prefixSums(rightLow) - prefixSums(leftIdx) < lower do rightLow += 1
        while rightHigh <= end && prefixSums(rightHigh) - prefixSums(leftIdx) <= upper do rightHigh += 1
        count += rightHigh - rightLow

        while rightIdx <= end && prefixSums(rightIdx) < prefixSums(leftIdx) do
          temp(mergeIdx) = prefixSums(rightIdx)
          mergeIdx += 1
          rightIdx += 1
        temp(mergeIdx) = prefixSums(leftIdx)
        mergeIdx += 1
      }

      while rightIdx <= end do
        temp(mergeIdx) = prefixSums(rightIdx)
        mergeIdx += 1
        rightIdx += 1
      (start to end).foreach(i => prefixSums(i) = temp(i))

    mergeSort(0, prefixSums.length - 1)
    count
