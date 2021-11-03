package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _785_IsGraphBipartite:
  def isBipartite(graph: Array[Array[Int]]): Boolean =
    val colors = Array.ofDim[Int](graph.length)

    graph.indices.foreach { node =>
      if colors(node) == 0 then
        val toVisit = mutable.Queue(node)
        colors(node) = 1

        while toVisit.nonEmpty do
          val current = toVisit.dequeue()
          graph(current).foreach { neighbour =>
            if colors(neighbour) == 0 then
              colors(neighbour) = -colors(current)
              toVisit.enqueue(neighbour)
            else if colors(neighbour) == colors(current) then return false
          }
    }
    true
