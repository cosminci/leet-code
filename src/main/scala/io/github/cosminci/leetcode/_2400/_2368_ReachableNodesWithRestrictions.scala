package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2368_ReachableNodesWithRestrictions:

  def reachableNodes(n: Int, edges: Array[Array[Int]], restricted: Array[Int]): Int =
    val restrictedSet = restricted.toSet

    val graph = edges
      .filter(_.toSet.intersect(restrictedSet).isEmpty)
      .foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) { case (graph, Array(n1, n2)) =>
        graph.updated(n1, graph(n1) :+ n2).updated(n2, graph(n2) :+ n1)
      }

    def dfs(curr: Int, parent: Int): Int =
      1 + graph(curr).collect { case child if child != parent => dfs(child, curr) }.sum

    dfs(curr = 0, parent = 0)
