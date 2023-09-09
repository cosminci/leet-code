package com.leetcode.cosminci._2900

object _2833_FurthestPointFromOrigin:

  def furthestDistanceFromOrigin(moves: String): Int =
    val (pos, budget) = moves.foldLeft(0, 0) { case ((pos, budget), move) =>
      move match
        case 'R' => (pos + 1, budget)
        case 'L' => (pos - 1, budget)
        case _   => (pos, budget + 1)
    }
    pos.abs + budget
