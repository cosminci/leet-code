package com.leetcode.cosminci._3000

object _2962_CountSubArrWhereMaxElemAppearsAtLeastKTimes:

  def countSubarrays(nums: Array[Int], k: Int): Long =
    val max = nums.max

    @annotation.tailrec
    def dfs(l: Int, r: Int, cnt: Int, res: Long): Long =
      if r == nums.length then res
      else
        val newCnt1 = cnt + (if nums(r) == max then 1 else 0)
        val (newCnt2, newL) = Iterator
          .iterate((newCnt1, l)) { case (cnt, l) => (cnt - (if nums(l) == max then 1 else 0), l + 1) }
          .dropWhile { case (cnt, _) => cnt == k }.next()
        dfs(newL, r + 1, newCnt2, res + newL)

    dfs(l = 0, r = 0, cnt = 0, res = 0)
