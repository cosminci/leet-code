package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.UnionFind

import scala.collection.mutable

object _2492_MinScoreOfPathBetweenTwoCities:

  def minScore(n: Int, roads: Array[Array[Int]]): Int =
    val uf = new UnionFind[Int]
    roads.foreach { case Array(x, y, _) => uf.union(x, y) }
    roads.foldLeft(Int.MaxValue) { case (minScore, Array(x, _, distance)) =>
      if uf.find(x) != uf.find(1) then minScore
      else minScore.min(distance)
    }
