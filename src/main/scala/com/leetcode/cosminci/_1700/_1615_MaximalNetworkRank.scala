package com.leetcode.cosminci._1700

object _1615_MaximalNetworkRank:

  def maximalNetworkRank(n: Int, roads: Array[Array[Int]]): Int =
    val graph = roads.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)) {
      case (acc, Array(u, v)) =>
      acc.updated(u, acc(u) + v).updated(v, acc(v) + u)
    }

    val networkRanks = for
      u <- 0 until n
      v <- u + 1 until n
    yield graph(u).size + graph(v).size - (if graph(u).contains(v) then 1 else 0)

    networkRanks.max
