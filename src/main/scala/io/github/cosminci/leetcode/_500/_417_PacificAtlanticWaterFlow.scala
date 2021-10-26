package io.github.cosminci.leetcode._500

import io.github.cosminci.utils

import scala.collection.mutable

object _417_PacificAtlanticWaterFlow:
  def main(args: Array[String]): Unit =
    println(
      pacificAtlantic(
        Array(
          Array(1, 2, 2, 3, 5),
          Array(3, 2, 3, 4, 4),
          Array(2, 4, 5, 3, 1),
          Array(6, 7, 1, 4, 5),
          Array(5, 1, 1, 2, 4)
        )
      )
    )

  private def pacificAtlantic(heights: Array[Array[Int]]): List[List[Int]] =
    val reachable = Array.ofDim[Int](heights.length, heights.head.length)

    val toVisitPacific = mutable.Queue.from {
      heights.indices.map(r => (r, 0)) ++
        heights.head.indices.map(c => (0, c))
    }
    while toVisitPacific.nonEmpty do
      val (x, y) = toVisitPacific.dequeue()
      if reachable(x)(y) != 1 then
        reachable(x)(y) = 1
        utils.neighbours(x, y, heights).foreach { case (nx, ny) =>
          if heights(nx)(ny) >= heights(x)(y) then toVisitPacific.enqueue((nx, ny))
        }

    val results = mutable.ListBuffer.empty[List[Int]]
    val toVisitAtlantic = mutable.Queue.from {
      heights.indices.map(r => (r, heights(r).length - 1)) ++
        heights.head.indices.map(c => (heights.length - 1, c))
    }
    while toVisitAtlantic.nonEmpty do
      val (x, y) = toVisitAtlantic.dequeue()
      if reachable(x)(y) != 2 && reachable(x)(y) != 3 then
        reachable(x)(y) = if reachable(x)(y) == 1 then 3 else 2
        if reachable(x)(y) == 3 then results.addOne(List(x, y))
        utils.neighbours(x, y, heights).foreach { case (nx, ny) =>
          if heights(nx)(ny) >= heights(x)(y) then toVisitAtlantic.enqueue((nx, ny))
        }

    results.toList
