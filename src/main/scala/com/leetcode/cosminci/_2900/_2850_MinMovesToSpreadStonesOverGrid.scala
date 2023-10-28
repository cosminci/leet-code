package com.leetcode.cosminci._2900

object _2850_MinMovesToSpreadStonesOverGrid:

  def minimumMoves(grid: Array[Array[Int]]): Int =
    def dist(x1: Int, y1: Int, x2: Int, y2: Int) =
      (x1 - x2).abs + (y1 - y2).abs

    val (zeroes, spare) = (0 until 3).foldLeft(Seq.empty[(Int, Int)], Seq.empty[(Int, Int)]) {
      case ((zeroes, spare), i) =>
        (0 until 3).foldLeft((zeroes, spare)) { case ((zeroes, spare), j) =>
          if grid(i)(j) == 1 then (zeroes, spare)
          else if grid(i)(j) == 0 then (zeroes :+ (i, j), spare)
          else (zeroes, spare ++ Seq.fill(grid(i)(j) - 1)((i, j)))
        }
    }

    spare.permutations.toSet.map { ones =>
      zeroes.zip(ones).map { case ((x1, y1), (x2, y2)) => dist(x1, y1, x2, y2) }.sum
    }.min
