package io.github.cosminci.leetcode._800

object _790_DominoAndTrominoTiling:
  private def numTilings(n: Int): Int =
    (1 to n)
      .foldLeft(-1L, 0L, 1L) { case ((prev3, prev2, prev1), _) =>
        (prev2, prev1, (2 * prev1 + prev3) % 1_000_000_007)
      }
      ._3
      .toInt
