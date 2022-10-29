package com.leetcode.cosminci._1400

import scala.collection.mutable

object _1306_JumpGameIII:

  def canReach(arr: Array[Int], start: Int): Boolean =
    val visited = mutable.Set(start)

    def canReachAfterJump(position: Int) =
      Option.when(arr.isDefinedAt(position) && !visited.contains(position)) {
        visited.add(position)
        position
      }.exists(dfs)

    def dfs(position: Int): Boolean =
      arr(position) == 0 ||
        canReachAfterJump(position + arr(position)) ||
        canReachAfterJump(position - arr(position))

    dfs(start)
