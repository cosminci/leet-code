package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils.UnionFind

object _2685_CountCompleteComponents:

  def countCompleteComponents(n: Int, edges: Array[Array[Int]]): Int =
    val uf = new UnionFind[Int]
    edges.foreach { case Array(u, v) => uf.union(u, v) }

    val nodeCountByRoot = (0 until n).groupMapReduce(uf.find)(_ => 1)(_ + _)
    val edgeCountByRoot = edges.groupMapReduce { case Array(u, _) => uf.find(u) }(_ => 1)(_ + _)

    nodeCountByRoot.count { case (root, nodeCount) =>
      edgeCountByRoot.get(root).forall(_ == nodeCount * (nodeCount - 1) / 2)
    }
