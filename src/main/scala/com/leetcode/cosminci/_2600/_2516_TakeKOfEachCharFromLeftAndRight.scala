package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2516_TakeKOfEachCharFromLeftAndRight:

  def takeCharacters(s: String, k: Int): Int =
    val limits = "abc".map(c => c -> (s.count(_ == c) - k)).toMap
    if "abc".exists(limits.getOrElse(_, 0) < 0) then return -1

    s.zipWithIndex
      .foldLeft(0, 0, Map.empty[Char, Int].withDefaultValue(0)) { case ((l, res, counter), (char, r)) =>
        Iterator
          .iterate((l, counter.updated(char, counter(char) + 1))) { case (l, counter) =>
            (l + 1, counter.updated(s(l), counter(s(l)) - 1))
          }
          .dropWhile { case (_, counter) => counter(char) > limits(char) }
          .next()
          .pipe { case (l, counter) => (l, res.max(r - l + 1), counter) }
      }
      .pipe { case (_, res, _) => s.length - res }
