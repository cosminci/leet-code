package com.leetcode.cosminci._100

object _88_MergeSortedArray:
  def main(args: Array[String]): Unit =
    val nums1 = Array(1, 2, 3, 0, 0, 0)
    merge(nums1, 3, Array(2, 5, 6), 3)
    println(nums1.toList)

  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit =
    var (i, j) = (m - 1, n - 1)
    while i >= 0 && j >= 0 do
      if nums1(i) <= nums2(j) then
        nums1(i + j + 1) = nums2(j)
        j -= 1
      else {
        nums1(i + j + 1) = nums1(i)
        i -= 1
      }
    while j >= 0 do
      nums1(j) = nums2(j)
      j -= 1
