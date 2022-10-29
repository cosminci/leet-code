package com.leetcode.cosminci._900

object _852_PeakIndexInMountainArray:
  def main(args: Array[String]): Unit =
    println(peakIndexInMountainArray(Array(0, 2, 1, 0)))
    println(peakIndexInMountainArray(Array(24, 69, 100, 99, 79, 78, 67, 36, 26, 19)))
    println(peakIndexInMountainArray(Array(3, 4, 5, 1)))

  def peakIndexInMountainArray(arr: Array[Int]): Int =
    var (l, r) = (0, arr.length - 1)
    while l < r do
      val mid = l + (r - l) / 2
      if arr(mid) < arr(mid + 1) then l = mid + 1
      else r = mid
    l
