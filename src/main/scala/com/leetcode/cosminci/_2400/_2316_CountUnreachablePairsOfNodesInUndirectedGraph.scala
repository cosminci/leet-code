package com.leetcode.cosminci._2400

import com.leetcode.cosminci.utils.UnionFind

object _2316_CountUnreachablePairsOfNodesInUndirectedGraph:

  def countPairs(n: Int, edges: Array[Array[Int]]): Long =
    val uf = new UnionFind[Int]
    edges.foreach { case Array(i, j) => uf.union(i, j) }
    val componentSizes = (0 until n).map(uf.find).distinct.map(uf.rank)
    if componentSizes.length == 1 then 0
    else componentSizes.map(size => size.toLong * (n - size)).sum / 2
