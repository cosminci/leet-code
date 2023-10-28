package com.leetcode.cosminci._2900

import scala.collection.mutable
import scala.util.chaining.*

object _2858_MinEdgeReversalsSoEveryNodeIsReachable:

  def minEdgeReversals(n: Int, edges: Array[Array[Int]]): Array[Int] =
    val res = Array.fill(n)(-1)

    val emptyGraph = Map.empty[Int, Map[Int, Int]].withDefaultValue(Map.empty.withDefaultValue(0))
    val graph = edges.foldLeft(emptyGraph) { case (graph, Array(i, j)) =>
      graph
        .updated(i, graph(i).updated(j, 0))
        .updated(j, graph(j).updated(i, 1))
    }

    def dp(i: Int, j: Int): Int =
      graph(j).collect { case (k, cost) if k != i => dp(j, k) + cost }.sum

    def dfs(i: Int, v: Int): Unit =
      (res(i) = v).tap { _ =>
        graph(i).foreach { case (j, cost) => if res(j) < 0 then dfs(j, v - cost + graph(j)(i)) }
      }

    dfs(i = 0, dp(i = -1, j = 0)).pipe(_ => res)
