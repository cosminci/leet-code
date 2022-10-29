package com.leetcode.cosminci._100

object _31_NextPermutation:

  def main(args: Array[String]): Unit =
    val arr = Array(1, 5, 8, 4, 7, 6, 5, 3, 1)
    nextPermutation(arr)
    println(arr.toList)

  def nextPermutation(nums: Array[Int]): Unit =
    val i = nums.zip(nums.drop(1)).lastIndexWhere { case (a, b) => a < b }
    if i >= 0 then swap(i, nums.lastIndexWhere(_ > nums(i)), nums)
    reverse(i + 1, nums)

  private def reverse(start: Int, nums: Array[Int]): Unit =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Unit =
      if l < r then
        swap(l, r, nums)
        dfs(l + 1, r - 1)
    dfs(l = start, r = nums.length - 1)

  private def swap(i: Int, j: Int, nums: Array[Int]): Unit =
    val tmp = nums(j)
    nums(j) = nums(i)
    nums(i) = tmp
