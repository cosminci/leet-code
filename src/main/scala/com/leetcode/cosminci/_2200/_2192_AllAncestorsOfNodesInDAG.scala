package com.leetcode.cosminci._2200

import scala.collection.mutable
import scala.collection.immutable.TreeSet

object _2192_AllAncestorsOfNodesInDAG {
  def getAncestors(n: Int, edges: Array[Array[Int]]): List[List[Int]] = {
    val adjList = edges.foldLeft(Map.empty[Int, TreeSet[Int]].withDefaultValue(TreeSet.empty[Int])) {
      case (adjList, Array(from, to)) =>
        adjList.updated(to, adjList(to) + from)
    }

    val mem = mutable.Map.empty[Int, TreeSet[Int]]
    def dfs(node: Int): TreeSet[Int] =
      mem.getOrElseUpdate(node, {
        if (adjList(node).isEmpty) TreeSet.empty
        else adjList(node).flatMap(ancestor => dfs(ancestor) + ancestor)
      })

    (0 until n).map(dfs(_).toList).toList
  }
}
