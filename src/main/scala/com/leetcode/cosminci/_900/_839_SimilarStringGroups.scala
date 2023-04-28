package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.UnionFind
import scala.util.chaining.*

object _839_SimilarStringGroups:

  def numSimilarGroups(strs: Array[String]): Int =
    def isSimilar(s1: String, s2: String) =
      s1.zip(s2)
        .count { case (c1, c2) => c1 != c2 }
        .pipe(cnt => cnt == 2 || cnt == 0)

    val uf = new UnionFind[String]

    for
      i <- 0 until strs.length - 1
      j <- i + 1 until strs.length
      if isSimilar(strs(i), strs(j))
    yield uf.union(strs(i), strs(j))

    strs.map(uf.find).distinct.length
