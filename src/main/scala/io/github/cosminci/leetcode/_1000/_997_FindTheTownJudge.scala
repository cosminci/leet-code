package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _997_FindTheTownJudge {
  private def findJudge(n: Int, trust: Array[Array[Int]]): Int = {
    val degree = Array.ofDim[Int](n + 1)
    trust.foreach { case Array(p1, p2) =>
      degree(p1) = degree(p1) - 1
      degree(p2) = degree(p2) + 1
    }
    degree.indexWhere(_ == n - 1, from = 1)
  }
}
