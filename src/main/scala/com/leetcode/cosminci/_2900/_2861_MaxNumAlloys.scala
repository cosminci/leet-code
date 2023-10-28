package com.leetcode.cosminci._2900

import scala.util.chaining.*

object _2861_MaxNumAlloys:

  def maxNumberOfAlloys(n: Int, k: Int, budget: Int, comps: List[List[Int]], stock: List[Int], cost: List[Int]): Int =
    comps.map { comp =>
      Iterator
        .iterate((0, Int.MaxValue / 2)) { case (l, r) =>
          val mid  = (l + r + 1) / 2
          val need = (0 until n).map { i => 0L.max(mid.toLong * comp(i) - stock(i)) * cost(i) }.sum
          if need <= budget then (mid, r) else (l, mid - 1)
        }
        .dropWhile { case (l, r) => l < r }.next()
        .pipe { case (l, _) => l }
    }.max
