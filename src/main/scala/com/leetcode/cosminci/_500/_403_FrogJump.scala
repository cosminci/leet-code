package com.leetcode.cosminci._500

import scala.collection.mutable

object _403_FrogJump:

  def canCross(stones: Array[Int]): Boolean =
    val validJumps = stones.toSet
    val mem        = mutable.Map.empty[(Int, Int), Boolean]
    def dfs(curr: Int, k: Int): Boolean = mem.getOrElseUpdate((curr, k), {
      curr == stones.last || Seq(k - 1, k, k + 1)
        .filter(jump => curr + jump != curr && validJumps.contains(curr + jump))
        .exists(jump => dfs(curr + jump, jump))
    })
    dfs(curr = stones.head, k = 0)
