package com.leetcode.cosminci._200

import scala.annotation.tailrec

object _153_FindMinimumInRotatedSortedArray:

  def main(args: Array[String]): Unit =
    List(
      Array(3, 1, 2),
      Array(1),
      Array(4, 5, 6, 7, 0, 1, 2),
      Array(3, 4, 5, 1, 2),
      Array(4, 5, 6, 7, 0, 1, 2),
      Array(11, 13, 15, 17)
    ).foreach { arr =>
      println(findMin(arr))
    }

  def findMin(nums: Array[Int]): Int =
    @tailrec
    def dfs(start: Int, end: Int): Int =
      if end - start <= 1 then return math.min(nums(start), nums(end))
      val mid = (end + start) / 2
      if nums(0) > nums(mid) then dfs(start, mid)
      else if nums(mid) > nums(end) then dfs(mid + 1, end)
      else nums(start)
      
    dfs(0, nums.length - 1)
