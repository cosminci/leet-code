package com.leetcode.cosminci._900

import scala.collection.mutable

object _847_ShortestPathVisitingAllNodes:

  def main(args: Array[String]): Unit =
    println(shortestPathLength(Array(Array(1, 2, 3), Array(0), Array(0), Array(0))))

  def shortestPathLength(graph: Array[Array[Int]]): Int =
    val toVisit = mutable.Queue.empty[NodeToVisit]
    val visited = mutable.Set.empty[VisitedNode]

    graph.indices.foreach { id =>
      toVisit.enqueue(NodeToVisit(id, 1 << id, 0))
      visited.add(VisitedNode(id, 1 << id))
    }

    while toVisit.nonEmpty do
      val NodeToVisit(id, visitedBitmask, cost) = toVisit.dequeue()
      if visitedBitmask == (1 << graph.length) - 1 then return cost

      graph(id).foreach { neighbour =>
        val newVisitedBitmask = visitedBitmask | (1 << neighbour)
        if !visited.contains(VisitedNode(neighbour, newVisitedBitmask)) then
          toVisit.enqueue(NodeToVisit(neighbour, newVisitedBitmask, cost + 1))
          visited.add(VisitedNode(neighbour, newVisitedBitmask))
      }

    Int.MinValue

  case class NodeToVisit(id: Int, visitedBitmask: Int, cost: Int)

  case class VisitedNode(id: Int, visitedBitmask: Int)
