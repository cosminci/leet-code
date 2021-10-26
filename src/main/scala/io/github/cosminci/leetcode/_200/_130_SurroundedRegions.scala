package io.github.cosminci.leetcode._200

import io.github.cosminci.utils

import scala.collection.mutable

object _130_SurroundedRegions:
  private def solve(board: Array[Array[Char]]): Unit =
    val (m, n) = (board.length, board.head.length)
    val sources =
      Seq(0, m - 1).flatMap(r => (0 until n).collect { case c if board(r)(c) == 'O' => (r, c) }) ++
        Seq(0, n - 1).flatMap(c => (0 until m).collect { case r if board(r)(c) == 'O' => (r, c) })

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

    board.indices.foreach { r =>
      board(r).indices.foreach { c =>
        if visited.contains((r, c)) then board(r)(c) = 'O'
        else board(r)(c) = 'X'
      }
    }
