package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2616_MinimizeMaxDiffOfPairs:

  def minimizeMax(nums: Array[Int], p: Int): Int =
    nums.sortInPlace()

    def check(diff: Int): Boolean =
      Iterator
        .iterate((1, 0)) { case (i, k) => if nums(i) - nums(i - 1) <= diff then (i + 2, k + 1) else (i + 1, k) }
        .dropWhile { case (i, k) => i < nums.length && k < p }.next()
        .pipe { case (_, k) => k >= p }

    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if check(mid) then search(l, mid)
        else search(mid + 1, r)

    search(l = 0, r = nums.last - nums.head)
