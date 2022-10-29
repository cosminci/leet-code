package com.leetcode.cosminci._2400

import scala.collection.mutable

object _2322_MinScoreAfterRemovalsFromTree:

  def minimumScore(nums: Array[Int], edges: Array[Array[Int]]): Int =
    val graph = edges.foldLeft(Map.empty[Int, Set[Int]].withDefaultValue(Set.empty)) {
      case (graph, Array(x, y)) =>
        graph.updated(x, graph(x) + y).updated(y, graph(y) + x)
    }
    val children = reduceGraph(graph)
    val xor      = children.view.mapValues(_.toSeq.map(nums).reduce(_ ^ _)).toMap

    val diffs = for
      x <- nums.indices.tail
      y <- x + 1 to nums.indices.last
    yield
      val scores =
        if children(y).contains(x) then Array(xor(x), xor(x) ^ xor(y), xor(0) ^ xor(y))
        else if children(x).contains(y) then Array(xor(y), xor(x) ^ xor(y), xor(0) ^ xor(x))
        else Array(xor(x), xor(y), xor(0) ^ xor(x) ^ xor(y))
      scores.max - scores.min

    diffs.min

  def reduceGraph(graph: Map[Int, Set[Int]]): Map[Int, Set[Int]] =
    val children = mutable.Map.empty[Int, mutable.Set[Int]].withDefaultValue(mutable.Set.empty)
    val visited  = mutable.Set(0)

    def dfs(curr: Int): Unit =
      children(curr) = mutable.Set(curr)
      graph(curr).diff(visited).foreach { next =>
        visited.add(next)
        dfs(next)
        children(curr).addAll(children(next))
      }

    dfs(curr = 0)
    children.view.mapValues(_.toSet).toMap.withDefaultValue(Set.empty)
