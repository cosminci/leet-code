package com.leetcode.cosminci._3000

object _2998_MinOpsToMakeXAndYEqual:

  def minimumOperationsToMakeEqual(x: Int, y: Int): Int =
    @annotation.tailrec
    def dfs(toVisit: Seq[Int], visited: Set[Int], steps: Int): Int =
      if toVisit.contains(y) then steps
      else
        val next = toVisit
          .flatMap(n => Seq(n + 1, n - 1) ++ Option.when(n % 11 == 0)(n / 11) ++ Option.when(n % 5 == 0)(n / 5))
          .filterNot(visited.contains)
        dfs(next, visited ++ next, steps + 1)

    dfs(toVisit = Seq(x), visited = Set(x), steps = 0)
