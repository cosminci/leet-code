package io.github.cosminci.leetcode._2300

import io.github.cosminci.utils

import scala.collection.mutable

object _2258_EscapeSpreadingFire:

  def maximumMinutes(grid: Array[Array[Int]]): Int =
    val (m, n) = (grid.length, grid.head.length)

    def bfs(start: Seq[(Int, Int)], timeGrid: Array[Array[Int]]): Array[Array[Int]] =
      val toVisit = mutable.Queue.from(start.map { case (x, y) => (x, y, 0) })
      while toVisit.nonEmpty do
        val (x, y, time) = toVisit.dequeue()
        timeGrid(x)(y) = time
        utils.neighbours(x, y, grid).foreach { case (nx, ny) =>
          if grid(nx)(ny) == 0 && timeGrid(nx)(ny) == -1 then toVisit.enqueue((nx, ny, time + 1))
        }
      timeGrid

    val timeGridSelf = bfs(start = Seq((0, 0)), timeGrid = Array.fill[Int](m, n)(-1))
    val fires        = (0 until m).flatMap(x => (0 until n).collect { case y if grid(x)(y) == 1 => (x, y) })
    val timeGridFire = bfs(start = fires, timeGrid = Array.fill[Int](m, n)(-1))

    val timeToArriveSelf = timeGridSelf(m - 1)(n - 1)
    val timeToArriveFire = timeGridFire(m - 1)(n - 1)

    if timeToArriveSelf == -1 then -1
    else if timeToArriveFire == -1 then 1_000_000_000
    else if timeToArriveFire < timeToArriveSelf then -1
    else
      val diff           = timeToArriveFire - timeToArriveSelf
      val (self1, self2) = (timeGridSelf(m - 1)(n - 2), timeGridSelf(m - 2)(n - 1))
      val (fire1, fire2) = (timeGridFire(m - 1)(n - 2), timeGridFire(m - 2)(n - 1))
      if self1 > -1 && self2 > -1 && (fire1 - self1 > diff || fire2 - self2 > diff) then diff else diff - 1
