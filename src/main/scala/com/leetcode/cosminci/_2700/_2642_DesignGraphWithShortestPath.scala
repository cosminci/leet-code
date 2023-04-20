package com.leetcode.cosminci._2700

import scala.collection.mutable
import scala.util.chaining.*

object _2642_DesignGraphWithShortestPath:

  class Graph(n: Int, edges: Array[Array[Int]]):

    private val graph =
      mutable.Map
        .empty[Int, Array[(Int, Int)]]
        .withDefaultValue(Array.empty)
        .tap { g => edges.foreach { case Array(from, to, cost) => g.update(from, g(from) :+ (to, cost)) } }

    def addEdge(edge: Array[Int]): Unit =
      val Array(from, to, cost) = edge
      graph.update(from, graph(from) :+ (to, cost))

    def shortestPath(node1: Int, node2: Int): Int =
      val toVisit = mutable.PriorityQueue((node1, 0))(Ordering.by { case (_, cost) => -cost })
      val visited = mutable.Map(node1 -> 0).withDefaultValue(Int.MaxValue)

      while toVisit.nonEmpty do
        val (curr, totalCost) = toVisit.dequeue()
        if curr == node2 then return totalCost
        graph(curr)
          .filter { case (next, cost) => totalCost + cost < visited(next) }
          .foreach { case (next, cost) =>
            visited.update(next, totalCost + cost)
            toVisit.enqueue((next, totalCost + cost))
          }

      -1
