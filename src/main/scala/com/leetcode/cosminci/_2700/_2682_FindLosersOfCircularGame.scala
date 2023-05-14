package com.leetcode.cosminci._2700

import scala.util.chaining.*

object _2682_FindLosersOfCircularGame:

  def circularGameLosers(n: Int, k: Int): Array[Int] =
    Iterator
      .iterate((Set.empty[Int], 0, 1)) { case (prev, curr, i) => (prev + curr, (curr + i * k) % n, i + 1) }
      .dropWhile { case (seen, next, _) => !seen.contains(next) }.next()
      .pipe { case (seen, _, _) => (1 to n).diff(seen.map(_ + 1).toSeq).toArray }
