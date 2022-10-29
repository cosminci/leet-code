package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _130_SurroundedRegions:
  def solve(board: Array[Array[Char]]): Unit =
    val (m, n) = (board.length, board.head.length)
    val sources = for
      r <- 0 until m
      c <- 0 until n
      if (r == 0 || r == m - 1 || c == 0 || c == n - 1) && board(r)(c) == 'O'
    yield (r, c)

    val toVisit = mutable.Stack.from(sources)
    val visited = mutable.Set.from(sources)

    while toVisit.nonEmpty do
      val (x, y)   = toVisit.pop()
      val adjacent = utils.neighbours(x, y, board)
      adjacent.foreach { case n @ (nx, ny) =>
        if !visited.contains(n) && board(nx)(ny) == 'O' then
          visited.add(n)
          toVisit.push(n)
      }

    for
      r <- 0 until m
      c <- 0 until n
      if !visited.contains((r, c))
    do board(r)(c) = 'X'
