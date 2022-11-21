package com.leetcode.cosminci._2500

import scala.util.chaining.*

object _2477_MinFuelCostToReportToCapital:

  def minimumFuelCost(roads: Array[Array[Int]], seats: Int): Long =
    val graph = roads.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(x, y)) =>
      graph.updated(x, graph(x) :+ y).updated(y, graph(y) :+ x)
    }

    def dfs(curr: Int, dest: Int): (Int, Long) =
      graph(curr)
        .filter(_ != dest)
        .foldLeft(1, 0L) { case ((ppl, ltrs), next) => dfs(next, curr).pipe { case (p, l) => (ppl + p, ltrs + l) } }
        .pipe { case (people, liters) => (people, liters + (people - 1) / seats + 1) }

    graph(0)
      .map(dfs(_, 0).pipe { case (_, liters) => liters })
      .sum
