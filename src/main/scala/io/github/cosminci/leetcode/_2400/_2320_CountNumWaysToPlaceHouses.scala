package io.github.cosminci.leetcode._2400

object _2320_CountNumWaysToPlaceHouses:

  def countHousePlacements(n: Int): Int =
    val oneSideCount = Iterator
      .iterate((1L, 1L)) { case (a, b) => (b, (a + b) % 1_000_000_007) }
      .drop(n)
      .next()
      ._2
    
    (oneSideCount * oneSideCount % 1_000_000_007).toInt
