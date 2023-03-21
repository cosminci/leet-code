package com.leetcode.cosminci._2600

object _2596_CheckKnightTour:

  def checkValidGrid(grid: Array[Array[Int]]): Boolean =
    val positions = for
      x <- grid.indices
      y <- grid(x).indices
    yield (grid(x)(y), x, y)

    grid(0)(0) == 0 && positions.sorted.sliding(2).forall { case Seq((i, x0, y0), (j, x1, y1)) =>
      j == i + 1 && x1 >= 0 && y1 >= 0 && x1 < grid.length && y1 < grid.length &&
        Set((x1 - x0).abs, (y1 - y0).abs) == Set(1, 2)
    }
