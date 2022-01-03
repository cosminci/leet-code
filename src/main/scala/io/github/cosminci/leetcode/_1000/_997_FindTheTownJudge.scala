package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _997_FindTheTownJudge:
  def findJudge(n: Int, trust: Array[Array[Int]]): Int =
    trust
      .foldLeft((1 to n).map(_ -> 0).toMap) { case (indegrees, Array(p1, p2)) =>
        indegrees.updated(p1, indegrees(p1) - 1).updated(p2, indegrees(p2) + 1)
      }
      .collectFirst { case (p, indegree) if indegree == n - 1 => p }
      .getOrElse(-1)
