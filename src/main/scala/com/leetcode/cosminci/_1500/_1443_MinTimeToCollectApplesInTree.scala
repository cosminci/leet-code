package com.leetcode.cosminci._1500

object _1443_MinTimeToCollectApplesInTree:

  def minTime(n: Int, edges: Array[Array[Int]], hasApple: List[Boolean]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(a, b)) =>
      graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }
    val appleNodes = hasApple.zipWithIndex.collect { case (has, node) if has => node }.toSet

    def dfs(node: Int, parent: Int): Int =
      val childCost = graph(node).filter(_ != parent).map(dfs(_, node)).sum
      if childCost > 0 || appleNodes.contains(node) then 2 + childCost else 0

    (dfs(node = 0, parent = -1) - 2).max(0)
