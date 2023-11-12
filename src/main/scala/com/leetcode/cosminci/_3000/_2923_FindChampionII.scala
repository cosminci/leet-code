package com.leetcode.cosminci._3000

import scala.util.chaining.*

object _2923_FindChampionII:

  def findChampion(n: Int, edges: Array[Array[Int]]): Int =
    edges
      .foldLeft((0 until n).map(_ -> 0).toMap) { (indegree, edge) => indegree.updated(edge(1), indegree(edge(1)) + 1) }
      .collect { case (node, indegree) if indegree == 0 => node }.toSeq
      .pipe(champions => if (champions.size == 1) champions.head else -1)
