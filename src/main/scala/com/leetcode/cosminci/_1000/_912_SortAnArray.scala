package com.leetcode.cosminci._1000

object _912_SortAnArray {
  def sortArray(nums: Array[Int]): Array[Int] = mergeSort(nums.toList).toArray

  private def mergeSort(nums: List[Int]): List[Int] =
    nums match {
      case head :: Nil => nums
      case _ =>
        val (fh, sh) = nums.splitAt(nums.length / 2)
        merge(mergeSort(fh), mergeSort(sh))
    }

  private def merge(nums1: List[Int], nums2: List[Int]): List[Int] =
    (nums1, nums2) match {
      case (Nil, _) => nums2
      case (_, Nil) => nums1
      case (h1 :: t1, h2 :: t2) =>
        if (h1 <= h2) h1 :: merge(t1, nums2) else h2 :: merge(nums1, t2)
    }
}
