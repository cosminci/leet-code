package com.leetcode.cosminci._1700

import com.leetcode.cosminci.utils.UnionFind
import scala.util.chaining.*

object _1697_CheckingExistenceOfEdgeLengthLimitedPaths:

  def distanceLimitedPathsExist(n: Int, edgeList: Array[Array[Int]], queryList: Array[Array[Int]]): Array[Boolean] =
    val edges   = edgeList.sortBy { case Array(_, _, dist) => dist }
    val queries = queryList.zipWithIndex.sortBy { case (Array(_, _, limit), _) => limit }

    val uf = new UnionFind[Int]
    queries
      .foldLeft(Map.empty[Int, Boolean], 0) { case ((res, j), (Array(u, v, limit), i)) =>
        Iterator
          .iterate(j) { j => uf.union(edges(j)(0), edges(j)(1)); j + 1 }
          .dropWhile { j => j < edges.length && edges(j)(2) < limit }.next()
          .pipe { j => (res.updated(i, uf.find(u) == uf.find(v)), j) }
      }.pipe { case (res, _) =>
        res.toArray.sortBy { case (i, _) => i }.map { case (_, bool) => bool }
      }
