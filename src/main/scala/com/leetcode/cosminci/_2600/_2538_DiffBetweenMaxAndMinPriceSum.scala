package com.leetcode.cosminci._2600

import scala.collection.mutable

object _2538_DiffBetweenMaxAndMinPriceSum:

  def maxOutput(n: Int, edges: Array[Array[Int]], price: Array[Int]): Long =
    val graph = edges.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) {
      case (graph, Array(a, b)) =>
        graph.updated(a, graph(a) :+ b).updated(b, graph(b) :+ a)
    }
    val mem = mutable.Map.empty[(Int, Int), Long]
    def dfs(curr: Int, prev: Int): Long = mem.getOrElseUpdate((curr, prev),
      graph(curr)
        .filterNot(_ == prev)
        .map(dfs(_, curr))
        .maxOption.getOrElse(0L) + price(curr)
    )
    (0 until n).map(node => dfs(node, -1) - price(node)).max
