package io.github.cosminci.leetcode._100

object _4_MedianOfSortedArrays:
  def main(args: Array[String]): Unit =
    println(findMedianSortedArrays(Array(1, 2), Array(3, 4)))

  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double =
    val (a, b) = if nums1.length > nums2.length then (nums1, nums2) else (nums2, nums1)
    val total  = a.length + b.length
    val half   = total / 2

    var (bLeftIdx, bRightIdx) = (0, b.length - 1)
    while true do
      val bMid = math.floor(bLeftIdx + (bRightIdx - bLeftIdx) / 2.0).toInt
      val aMid = half - bMid - 2

      val aLeft  = if aMid >= 0 then a(aMid) else Int.MinValue
      val aRight = if aMid + 1 < a.length then a(aMid + 1) else Int.MaxValue
      val bLeft  = if bMid >= 0 then b(bMid) else Int.MinValue
      val bRight = if bMid + 1 < b.length then b(bMid + 1) else Int.MaxValue

      if bLeft <= aRight && aLeft <= bRight then
        if total % 2 == 0 then
          return (
            math.max(bLeft, aLeft) +
              math.min(bRight, aRight)
          ) / 2.0
        else return math.min(aRight, bRight)
      else if bLeft > aRight then bRightIdx = bMid - 1
      else bLeftIdx = bMid + 1
    0
