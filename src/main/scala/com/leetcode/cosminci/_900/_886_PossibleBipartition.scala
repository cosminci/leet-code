package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.UnionFind

object _886_PossibleBipartition {
  def possibleBipartition(n: Int, dislikes: Array[Array[Int]]): Boolean = {
    val adjList = dislikes.foldLeft(Map.empty[Int, Seq[Int]].withDefaultValue(Seq.empty)) {
      case (acc, Array(n1, n2)) =>
        acc.updated(n1, acc(n1) :+ n2).updated(n2, acc(n2) :+ n1)
    }

    val uf = new UnionFind[Int]
    (1 to n + 1).forall { curr =>
      if (adjList(curr).isEmpty) true
      else adjList(curr).reduce(uf.union) != uf.find(curr)
    }
  }
}
