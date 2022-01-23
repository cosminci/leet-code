package io.github.cosminci.leetcode._2200

import scala.collection.mutable

object _2139_MinMovesToReachTargetScore {

  def minMoves(target: Int, maxDoubles: Int): Int = {
    @annotation.tailrec
    def dfs(n: Int, doubles: Int, moves: Int): Int =
      if (n == 1) moves
      else if (doubles == maxDoubles) moves + n - 1
      else if (n % 2 == 1) dfs(n - 1, doubles, moves + 1)
      else dfs(n / 2, doubles + 1, moves + 1)

    dfs(n = target, doubles = 0, moves = 0)
  }
}
