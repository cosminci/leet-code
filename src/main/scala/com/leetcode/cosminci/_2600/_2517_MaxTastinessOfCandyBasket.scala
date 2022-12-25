package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2517_MaxTastinessOfCandyBasket:

  def maximumTastiness(price: Array[Int], k: Int): Int =
    price.sortInPlace()
    Iterator
      .iterate((0, price.last - price.head)) { case (l, r) =>
        val mid = (l + r + 1) / 2
        val (cnt, _) = (1 until price.length).foldLeft(1, 0) { case ((cnt, i), j) =>
          if price(j) - price(i) < mid then (cnt, i)
          else (cnt + 1, j)
        }
        if cnt >= k then (mid, r)
        else (l, mid - 1)
      }
      .dropWhile { case (l, r) => l < r }.next()
      .pipe { case (l, r) => l }
