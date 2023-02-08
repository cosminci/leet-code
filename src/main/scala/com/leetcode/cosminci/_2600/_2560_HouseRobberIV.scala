package com.leetcode.cosminci._2600

object _2560_HouseRobberIV:

  def minCapability(nums: Array[Int], k: Int): Int =
    @annotation.tailrec
    def search(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2

        val (taken, _) = Iterator
          .iterate((0, 0)) { case (taken, i) => if nums(i) > mid then (taken, i + 1) else (taken + 1, i + 2) }
          .dropWhile { case (taken, i) => taken < k && i < nums.length }.next()

        if taken >= k then search(l, mid)
        else search(mid + 1, r)

    search(l = nums.min, r = nums.max)
