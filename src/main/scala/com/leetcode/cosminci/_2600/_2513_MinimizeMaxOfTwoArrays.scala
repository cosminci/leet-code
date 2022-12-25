package com.leetcode.cosminci._2600

import com.leetcode.cosminci.utils

object _2513_MinimizeMaxOfTwoArrays:

  def minimizeSet(divisor1: Int, divisor2: Int, uniqueCnt1: Int, uniqueCnt2: Int): Int =
    val lcm = utils.lcm(divisor1.toLong, divisor2.toLong)

    @annotation.tailrec
    def dfs(l: Long, r: Long): Long =
      if l >= r then l
      else
        val mid     = l + (r - l) / 2
        val div1Cnt = mid - mid / divisor1
        val div2Cnt = mid - mid / divisor2
        val lcmCnt  = mid - mid / lcm
        if div1Cnt < uniqueCnt1 || div2Cnt < uniqueCnt2 || lcmCnt < uniqueCnt1 + uniqueCnt2 then dfs(mid + 1, r)
        else dfs(l, mid)

    dfs(l = 0, r = Int.MaxValue).toInt
