package com.leetcode.cosminci._800

import scala.util.chaining.*

object _790_DominoAndTrominoTiling:

  def numTilings(n: Int): Int =
    Iterator
      .iterate((-1L, 0L, 1L)) { case (prev3, prev2, prev1) => (prev2, prev1, (2 * prev1 + prev3) % 1_000_000_007) }
      .drop(n).next()
      .pipe { case (_, _, res) => res.toInt }
