package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2551_PutMarblesInBags:

  def putMarbles(weights: Array[Int], k: Int): Long =
    weights
      .map(_.toLong)
      .sliding(2)
      .map(_.sum)
      .toSeq
      .sorted
      .pipe(scores => scores.takeRight(k - 1).sum - scores.take(k - 1).sum)
