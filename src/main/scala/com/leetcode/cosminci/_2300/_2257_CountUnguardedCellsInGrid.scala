package com.leetcode.cosminci._2300

object _2257_CountUnguardedCellsInGrid:

  def countUnguarded(m: Int, n: Int, guards: Array[Array[Int]], walls: Array[Array[Int]]): Int =
    val grid       = Array.fill(m, n)(0)
    val directions = Array((0, 1), (1, 0), (-1, 0), (0, -1))

    (guards ++ walls).foreach { case Array(r, c) => grid(r)(c) = 1 }
    def inGrid(r: Int, c: Int, dr: Int, dc: Int) = r + dr >= 0 && r + dr < m && c + dc >= 0 && c + dc < n

    for
      Array(r, c) <- guards
      (dr, dc)    <- directions
    do
      Iterator
        .iterate((r, c)) { case (r, c) => (r + dr, c + dc) }
        .takeWhile { case (r, c) => inGrid(r, c, dr, dc) && grid(r + dr)(c + dc) != 1 }
        .foreach { case (r, c) => grid(r + dr)(c + dc) = 2 }

    grid.map(r => r.count(_ == 0)).sum
