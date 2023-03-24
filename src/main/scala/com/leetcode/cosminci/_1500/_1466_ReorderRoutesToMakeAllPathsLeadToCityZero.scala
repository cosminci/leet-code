package com.leetcode.cosminci._1500

object _1466_ReorderRoutesToMakeAllPathsLeadToCityZero:

  def minReorder(n: Int, connections: Array[Array[Int]]): Int =
    val graph = connections.foldLeft(Map.empty[Int, Seq[(Int, Int)]].withDefaultValue(Seq.empty)) {
      case (g, Array(a, b)) =>
        g.updated(a, g(a) :+ (b, 1)).updated(b, g(b) :+ (a, 0))
    }

    def dfs(curr: Int, prev: Int): Int =
      graph(curr)
        .filterNot { case (next, _) => next == prev }
        .map { case (next, cost) => cost + dfs(next, curr) }
        .sum

    dfs(curr = 0, prev = -1)
