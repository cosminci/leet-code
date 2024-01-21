package com.leetcode.cosminci._3100

object _3011_FindIfArrayCanBeSorted:

  def main(args: Array[String]): Unit =
    println(canSortArray(Array(8, 4, 2, 30, 15)))

  def canSortArray(nums: Array[Int]): Boolean =
    @annotation.tailrec
    def dfs(nums: Array[Int], res: Array[Array[Int]]): Array[Array[Int]] =
      if nums.isEmpty then res
      else
        val splitAt = nums.indices
          .find(i => i > 0 && Integer.bitCount(nums(i)) != Integer.bitCount(nums(i - 1)))
          .getOrElse(nums.length)
        val (fh, sh) = nums.splitAt(splitAt)
        dfs(sh, res :+ fh)

    dfs(nums, res = Array.empty).flatMap(_.sorted) sameElements nums.sorted
