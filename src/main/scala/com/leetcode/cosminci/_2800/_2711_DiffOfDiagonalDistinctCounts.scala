package com.leetcode.cosminci._2800

import scala.util.chaining._

object _2711_DiffOfDiagonalDistinctCounts:

  def differenceOfDistinctValues(g: Array[Array[Int]]): Array[Array[Int]] =
    val (m, n) = (g.length, g.head.length)
    val res    = Array.fill(m, n)(0)

    ((0 until n).map(j => (0, j)) ++ (1 until m).map(i => (i, 0))).foreach { case (i, j) =>
      Iterator.iterate((0, Set.empty[Int])) { case (d, tl) =>
        (d + 1, tl + g(i + d)(j + d)).tap(_ => res(i + d)(j + d) = tl.size)
      }.dropWhile { case (d, _) => i + d < m && j + d < n }.next()

      Iterator.iterate(((m - i).min(n - j) - 1, Set.empty[Int])) { case (d, br) =>
        (d - 1, br + g(i + d)(j + d)).tap(_ => res(i + d)(j + d) = (res(i + d)(j + d) - br.size).abs)
      }.dropWhile { case (d, _) => d >= 0 }.next()
    }.pipe(_ => res)
