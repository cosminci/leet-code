package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _373_FindKPairsWithSmallestSums:
  def main(args: Array[String]): Unit =
    val nums1 = Array(1, 4, 5, 6, 7)
    val nums2 = Array(1, 2, 3, 4, 5)
    println(kSmallestPairs(nums1, nums2, 30))

  def kSmallestPairs(nums1: Array[Int], nums2: Array[Int], k: Int): List[List[Int]] =
    val next =
      given Ordering[(Int, Int)] = (indexTuple1, indexTuple2) =>
        (nums1(indexTuple2._1) + nums2(indexTuple2._2)).compare(nums1(indexTuple1._1) + nums2(indexTuple1._2))
      mutable.PriorityQueue((0, 0))

    val result = mutable.ListBuffer.empty[List[Int]]
    while next.nonEmpty && result.length < k do
      val (nums1Idx, nums2Idx) = next.dequeue()
      result.append(List(nums1(nums1Idx), nums2(nums2Idx)))

      if nums2Idx < nums2.length - 1 then next.enqueue((nums1Idx, nums2Idx + 1))
      if nums2Idx == 0 && nums1Idx < nums1.length - 1 then next.enqueue((nums1Idx + 1, nums2Idx))

    result.toList
