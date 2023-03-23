package com.leetcode.cosminci._1400

import com.leetcode.cosminci.utils.UnionFind

object _1319_NumOpsToMakeNetworkConnected:

  def makeConnected(n: Int, connections: Array[Array[Int]]): Int =
    if connections.length < n - 1 then -1
    else
      val uf = new UnionFind[Int]
      connections.foreach { case Array(a, b) => uf.union(a, b) }
      (0 until n).groupBy(uf.find).size - 1
