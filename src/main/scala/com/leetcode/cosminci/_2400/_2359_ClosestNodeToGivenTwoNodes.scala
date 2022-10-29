package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2359_ClosestNodeToGivenTwoNodes:

  def closestMeetingNode(edges: Array[Int], node1: Int, node2: Int): Int =
    @annotation.tailrec
    def dfs(distances: Array[Int], curr: Int, distance: Int): Array[Int] =
      distances(curr) = distance
      val next = edges(curr)
      if next == -1 || distances(next) != Int.MaxValue then distances
      else dfs(distances, next, distance + 1)

    val node1Distances = dfs(Array.fill(edges.length)(Int.MaxValue), node1, distance = 0)
    val node2Distances = dfs(Array.fill(edges.length)(Int.MaxValue), node2, distance = 0)

    node1Distances
      .zip(node2Distances)
      .zipWithIndex
      .collect { case ((d1, d2), node) if d1 != Int.MaxValue && d2 != Int.MaxValue => (d1.max(d2), node) }
      .minOption
      .map { case (_, node) => node }
      .getOrElse(-1)
