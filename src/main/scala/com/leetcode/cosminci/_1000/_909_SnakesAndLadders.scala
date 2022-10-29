package com.leetcode.cosminci._1000

import scala.collection.mutable
import scala.math.Integral.Implicits.*

object _909_SnakesAndLadders:
  def main(args: Array[String]): Unit =
    println(
      snakesAndLadders(
        Array(
          Array(-1, -1, 19, 10, -1),
          Array(2, -1, -1, 6, -1),
          Array(-1, 17, -1, 19, -1),
          Array(25, -1, 20, -1, -1),
          Array(-1, -1, -1, -1, 15)
        )
      )
    )

  def snakesAndLadders(board: Array[Array[Int]]): Int =
    val n       = board.length
    val toVisit = mutable.Queue((1, 0))
    val visited = mutable.Set(1)

    def labelToCoords(label: Int): (Int, Int) =
      val (r, c) = (label - 1) /% n
      if r % 2 == 0 then (n - 1 - r, c) else (n - 1 - r, n - 1 - c)

    while toVisit.nonEmpty do
      val (curr, steps) = toVisit.dequeue()
      if curr == n * n then return steps

      ((curr + 1) to (curr + 6).min(n * n)).foreach { label =>
        val (r, c) = labelToCoords(label)
        val next   = if board(r)(c) > 0 then board(r)(c) else label
        if !visited.contains(next) then
          visited.add(next)
          toVisit.enqueue((next, steps + 1))
      }

    -1
