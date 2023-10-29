package com.leetcode.cosminci._2900

import scala.collection.mutable

object _2896_ApplyOpsToMakeStringsEqual:

  def minOperations(s1: String, s2: String, x: Int): Int =
    val diffs = s1.indices.filter(i => s1(i) != s2(i))

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(l: Int, r: Int): Int = mem.getOrElseUpdate((l, r),
      if l >= r then 0
      else
        val moveRight = x.min(diffs(l + 1) - diffs(l)) + dfs(l + 2, r)
        val moveMid   = x.min(diffs(r) - diffs(l)) + dfs(l + 1, r - 1)
        val moveLeft  = x.min(diffs(r) - diffs(r - 1)) + dfs(l, r - 2)
        moveRight.min(moveMid).min(moveLeft)
    )

    if diffs.length % 2 == 1 then -1
    else if diffs.isEmpty then 0
    else dfs(l = 0, r = diffs.length - 1)
