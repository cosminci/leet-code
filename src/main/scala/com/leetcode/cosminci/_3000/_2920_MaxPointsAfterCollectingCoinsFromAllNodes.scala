package com.leetcode.cosminci._3000

import scala.collection.mutable
import scala.util.chaining.*

object _2920_MaxPointsAfterCollectingCoinsFromAllNodes:

  def maximumPoints(edges: Array[Array[Int]], coins: Array[Int], k: Int): Int =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { (graph, edge) =>
      graph
        .updated(edge(0), graph(edge(0)) :+ edge(1))
        .updated(edge(1), graph(edge(1)) :+ edge(0))
    }

    val mem = mutable.Map.empty[(Int, Int), Int]
    def dfs(curr: Int, parent: Int, penalty: Int): Int = mem.getOrElseUpdate((curr, penalty), {
      val value = coins(curr) / (1 << penalty)
      graph(curr).foldLeft(value - k, value / 2) { case ((opt1, opt2), next) =>
        if next == parent then (opt1, opt2)
        else (
          opt1 + dfs(next, curr, penalty),
          if penalty > 12 then opt2 else opt2 + dfs(next, curr, penalty + 1)
        )
      }.pipe { case (opt1, opt2) => opt1.max(opt2) }
    })

    dfs(curr = 0, parent = -1, penalty = 0)
