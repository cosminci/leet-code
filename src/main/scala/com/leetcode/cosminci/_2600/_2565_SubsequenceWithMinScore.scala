package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2565_SubsequenceWithMinScore:

  def minimumScore(s: String, t: String): Int =
    s.indices.foldLeft(0) { (j, i) =>
      val newJ = if s(i) == t(j) then j + 1 else j
      if newJ < t.length then newJ else return 0
    }

    val firstRemovedFromLeft = s.indices
      .foldLeft(Array.empty[Int], 0) { case ((firstRemovedFromLeft, left), i) =>
        val newLeft = if s(i) == t(left) then left + 1 else left
        (firstRemovedFromLeft :+ newLeft, newLeft)
      }
      .pipe { case (firstRemovedFromLeft, _) => firstRemovedFromLeft }

    s.indices
      .foldRight(t.length, t.length - 1) { case (i, (result, right)) =>
        val newRight = if s(i) == t(right) then right - 1 else right
        if right < firstRemovedFromLeft(i) then (result.min(newRight + 1), newRight)
        else (result.min(newRight + 1).min(right - firstRemovedFromLeft(i) + 1), newRight)
      }
      .pipe { case (result, _) => result }
