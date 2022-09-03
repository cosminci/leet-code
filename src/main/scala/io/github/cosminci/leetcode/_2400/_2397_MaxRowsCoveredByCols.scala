package io.github.cosminci.leetcode._2400

object _2397_MaxRowsCoveredByCols:

  def maximumRows(mat: Array[Array[Int]], cols: Int): Int =
    val (m, n) = (mat.length, mat.head.length)

    val bitmasks = (0 until m).map { r =>
      (0 until n).foldLeft(0)((mask, c) => if mat(r)(c) == 0 then mask else mask | (1 << c))
    }

    (0 until n).combinations(cols).map { cols =>
      val cancellingMask = (0 until n).diff(cols).foldLeft(0)((mask, c) => mask | (1 << c))
      bitmasks.count(mask => (mask & cancellingMask) == 0)
    }.max
