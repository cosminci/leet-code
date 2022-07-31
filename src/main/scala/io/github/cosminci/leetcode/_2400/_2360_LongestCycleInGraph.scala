package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2360_LongestCycleInGraph:

  def longestCycle(edges: Array[Int]): Int =
    edges.indices.foldLeft(Set.empty[Int], -1) { case ((globalVisited, longestCycle), node) =>
      def dfs(node: Int): (Set[Int], Int) = {
        val (last, path) = Iterator
          .iterate((node, Map.empty[Int, Int])) { case (curr, path) => (edges(curr), path + (curr -> path.size)) }
          .dropWhile { case (curr, path) => curr != -1 && !path.contains(curr) && !globalVisited.contains(curr) }
          .next()
        (path.keySet, path.get(last).map(path.size - _).getOrElse(-1))
      }
      val (localVisited, cycleLength) = dfs(node)
      (globalVisited ++ localVisited, longestCycle.max(cycleLength))
    }._2
