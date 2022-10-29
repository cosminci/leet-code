package com.leetcode.cosminci._100

object _35_SearchInsertPosition:
  def main(args: Array[String]): Unit =
    println(searchInsert(Array(1, 3, 5, 6), 5))

  def searchInsert(nums: Array[Int], target: Int): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then l
      else {
        val mid = l + (r - l) / 2
        if nums(mid) >= target then dfs(l, mid)
        else dfs(mid + 1, r)
      }
    dfs(l = 0, r = nums.length)
