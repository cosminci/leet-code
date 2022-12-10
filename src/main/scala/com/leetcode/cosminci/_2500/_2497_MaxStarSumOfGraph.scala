package com.leetcode.cosminci._2500

object _2497_MaxStarSumOfGraph:

  def maxStarSum(vals: Array[Int], edges: Array[Array[Int]], k: Int): Int =
    val bestEdges = edges.foldLeft(Map.empty[Int, Array[Int]].withDefaultValue(Array.empty[Int])) {
      case (bestEdges, Array(a, b)) =>
        bestEdges
          .updated(a, bestEdges(a) :+ vals(b))
          .updated(b, bestEdges(b) :+ vals(a))
    }
    vals.zipWithIndex
      .map { case (value, node) =>
        bestEdges(node)
          .sorted
          .takeRight(k)
          .filter(_ > 0)
          .sum + value
      }.max
