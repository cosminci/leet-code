package com.leetcode.cosminci._3100

import scala.util.chaining._

object _3015_CountHousesAtCertainDistanceI:

  def countOfPairs(n: Int, x: Int, y: Int): Array[Int] =
    Array.fill(n)(0).tap { res =>
      (1 to n).combinations(2).foreach { case Seq(l, r) =>
        val a = (l - r).abs
        val b = (l - x).abs + (r - y).abs + 1
        val c = (l - y).abs + (r - x).abs + 1
        res(a.min(b).min(c) - 1) += 2
      }
    }
