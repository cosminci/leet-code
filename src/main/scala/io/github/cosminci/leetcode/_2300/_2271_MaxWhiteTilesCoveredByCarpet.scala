package io.github.cosminci.leetcode._2300

object _2271_MaxWhiteTilesCoveredByCarpet:

  def maximumWhiteTiles(tilesArr: Array[Array[Int]], carpetLen: Int): Int =
    val tiles     = tilesArr.map { case Array(l, r) => (l, r) }.sorted
    val prefixSum = tiles.scanLeft(0) { case (acc, (l, r)) => acc + r - l + 1 }
    val ends      = tiles.map { case (_, r) => r }

    tiles.indices
      .foldLeft(0, 0) { case ((maxTiles, j0), i) =>
        val (l, _) = tiles(i)
        val r      = ends.last.min(l + carpetLen - 1)
        val j      = Iterator.iterate(j0)(_ + 1).dropWhile(j => j < tiles.length && ends(j) < r).next()

        if tiles(j)._1 > r then (maxTiles.max(prefixSum(j) - prefixSum(i)), j)
        else (maxTiles.max(prefixSum(j + 1) - prefixSum(i) - ends(j) + r), j)
      }._1
