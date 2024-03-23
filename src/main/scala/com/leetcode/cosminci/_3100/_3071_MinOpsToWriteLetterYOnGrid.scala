package com.leetcode.cosminci._3100

object _3071_MinOpsToWriteLetterYOnGrid:

  def minimumOperationsToWriteY(grid: Array[Array[Int]]): Int =
    val (n, n2) = (grid.length, grid.length / 2)
    val yValues = (0 until n2).flatMap(i => Seq(grid(i)(i), grid(n - i - 1)(n2), grid(i)(n - i - 1))) :+ grid(n2)(n2)

    val withinYCounter  = yValues.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)
    val gridCounter     = grid.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    val outsideYCounter = gridCounter.map { case (v, cnt) => v -> (cnt - withinYCounter(v)) }.withDefaultValue(0)

    n * n - Seq((0, 1), (0, 2), (1, 0), (1, 2), (2, 0), (2, 1))
      .map { case (yValue, nonYValue) => withinYCounter(yValue) + outsideYCounter(nonYValue) }.max
