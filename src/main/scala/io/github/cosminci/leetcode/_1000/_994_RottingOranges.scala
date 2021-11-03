package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils

import scala.collection.mutable

object _994_RottingOranges:
  def main(args: Array[String]): Unit =
    println(orangesRotting(Array(Array(1, 2))))

  def orangesRotting(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)

    val rottenSources = for
      x <- 0 until m
      y <- 0 until n
      if grid(x)(y) == 2
    yield (x, y, 0)

    var minDaysNeeded = 0
    val toVisit       = mutable.Queue.from(rottenSources)
    while toVisit.nonEmpty do
      val (x, y, daysNeeded) = toVisit.dequeue()
      minDaysNeeded = math.max(minDaysNeeded, daysNeeded)
      utils.neighbours(x, y, grid).foreach { case (nx, ny) =>
        if grid(nx)(ny) == 1 then
          grid(nx)(ny) = -1
          toVisit.enqueue((nx, ny, daysNeeded + 1))
      }

    if (0 until m).exists(x => (0 until n).exists(y => grid(x)(y) == 1)) then -1 else minDaysNeeded
