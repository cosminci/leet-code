package com.leetcode.cosminci._2600

object _2552_CountIncreasingQuadruplets:

  def countQuadruplets(nums: Array[Int]): Long =
    val n = nums.length
    val left = (1 until n).scanLeft(Array.fill(n + 1)(0)) { (left, i) =>
      Array.tabulate(n + 1)(num => if nums(i - 1) < num then left(num) + 1 else left(num))
    }
    val right = (0 until n - 1).scanRight(Array.fill(n + 1)(0)) { (l, right) =>
      Array.tabulate(n + 1)(num => if nums(l + 1) > num then right(num) + 1 else right(num))
    }
    (0 until n - 2).foldLeft(0L) { (res, j) =>
      (j + 1 until n).foldRight(res) { (k, res) =>
        if nums(k) < nums(j) then res + left(j)(nums(k)).toLong * right(k)(nums(j))
        else res
      }
    }
