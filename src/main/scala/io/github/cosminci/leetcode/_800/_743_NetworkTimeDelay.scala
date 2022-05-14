package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _743_NetworkTimeDelay:

  def networkDelayTime(times: Array[Array[Int]], n: Int, k: Int): Int =
    val dist = Array.tabulate(n + 1)(i => if i == k then 0 else Int.MaxValue)

    for
      _              <- 0 until n
      Array(u, v, w) <- times
      if dist(u) != Int.MaxValue
    do dist(v) = dist(v).min(dist(u) + w)

    val maxDist = dist.tail.max
    if maxDist < Int.MaxValue then maxDist else -1
