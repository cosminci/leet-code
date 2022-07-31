package io.github.cosminci.leetcode._2400

import scala.collection.mutable

object _2359_ClosestNodeToGivenTwoNodes:

  def closestMeetingNode(edges: Array[Int], node1: Int, node2: Int): Int =
    def bfs(toVisit: mutable.Queue[(Int, Int)], distances: Array[Int]): Unit =
      while toVisit.nonEmpty do
        val (curr, distance) = toVisit.dequeue()
        distances(curr) = distance
        val next = edges(curr)
        if next != -1 && distances(next) == Int.MaxValue then toVisit.enqueue((next, distance + 1))

    val node1Distances = Array.fill(edges.length)(Int.MaxValue)
    val node2Distances = Array.fill(edges.length)(Int.MaxValue)
    bfs(mutable.Queue((node1, 0)), node1Distances)
    bfs(mutable.Queue((node2, 0)), node2Distances)

    node1Distances
      .zip(node2Distances)
      .zipWithIndex
      .collect { case ((d1, d2), node) if d1 != Int.MaxValue && d2 != Int.MaxValue => (d1.max(d2), node) }
      .minOption
      .map { case (_, node) => node }
      .getOrElse(-1)
