package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _802_FindEventualSafeStates:
  def eventualSafeNodes(graph: Array[Array[Int]]): List[Int] =
    val mem     = mutable.Map.empty[Int, Boolean]
    val visited = mutable.Set.empty[Int]

    def isSafe(node: Int): Boolean =
      mem.getOrElseUpdate(node, {
        if visited.contains(node) then false
        else {
          visited.add(node)
          val result = graph(node).forall(isSafe)
          visited.remove(node)
          result
        }
      })

    (0 until graph.length).filter(isSafe).toList
