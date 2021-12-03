package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils.UnionFind

object _947_MostStonesRemovedWithSameRowOrColumn {
  def removeStones(stones: Array[Array[Int]]): Int = {
    val uf = new UnionFind[Int]
    stones.foreach { case Array(x, y) => uf.union(x, y + 10000) }
    stones.length - uf.nodes().map(uf.find).size
  }
}
