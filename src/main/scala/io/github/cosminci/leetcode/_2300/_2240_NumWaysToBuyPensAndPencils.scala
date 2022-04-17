package io.github.cosminci.leetcode._2300

object _2240_NumWaysToBuyPensAndPencils:

  def waysToBuyPensPencils(total: Int, cost1: Int, cost2: Int): Long =
    Iterator
      .iterate(0)(_ + 1)
      .takeWhile(_ * cost1 <= total)
      .map(pens => (total - pens * cost1) / cost2 + 1L)
      .sum
