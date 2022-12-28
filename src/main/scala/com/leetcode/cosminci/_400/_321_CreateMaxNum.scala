package com.leetcode.cosminci._400

import scala.util.chaining.*

object _321_CreateMaxNum:

  def maxNumber(nums1: Array[Int], nums2: Array[Int], k: Int): Array[Int] =
    val (n, m) = (nums1.length, nums2.length)
    ((k - m).max(0) to k.min(n))
      .map(i => merge(maxArray(nums1, i), i = 0, maxArray(nums2, k - i), j = 0, k))
      .sortWith { case (nums1, nums2) => greater(nums1, i = 0, nums2, j = 0) }
      .head

  private def merge(nums1: Array[Int], i: Int, nums2: Array[Int], j: Int, k: Int): Array[Int] =
    Iterator
      .iterate((i, j, Array.empty[Int])) { case (i, j, res) =>
        if greater(nums1, i, nums2, j) then (i + 1, j, res :+ nums1(i))
        else (i, j + 1, res :+ nums2(j))
      }
      .dropWhile { case (i, j, _) => i + j < k }
      .next()
      .pipe { case (_, _, res) => res }

  private def greater(nums1: Array[Int], i: Int, nums2: Array[Int], j: Int) =
    Iterator
      .iterate((i, j)) { case (i, j) => (i + 1, j + 1) }
      .dropWhile { case (i, j) => i < nums1.length && j < nums2.length && nums1(i) == nums2(j) }
      .next()
      .pipe { case (i, j) => j == nums2.length || (i < nums1.length && nums1(i) > nums2(j)) }

  private def maxArray(nums: Array[Int], k: Int) =
    nums.indices.foldLeft(Array.empty[Int]) { (stack, i) =>
      Iterator
        .iterate(stack)(_.dropRight(1))
        .dropWhile(stack => stack.length + nums.length - i > k && stack.lastOption.exists(_ < nums(i)))
        .next()
        .pipe(stack => if stack.length < k then stack :+ nums(i) else stack)
    }
