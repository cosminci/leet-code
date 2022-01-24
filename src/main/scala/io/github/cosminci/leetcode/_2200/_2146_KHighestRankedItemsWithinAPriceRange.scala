package io.github.cosminci.leetcode._2200

import io.github.cosminci.utils

import scala.collection.mutable

object _2146_KHighestRankedItemsWithinAPriceRange:
  def main(args: Array[String]): Unit =
    println(highestRankedKItems(Array(Array(1, 1, 1), Array(0, 0, 1), Array(2, 3, 4)), Array(2, 3), Array(0, 0), 3))

  def highestRankedKItems(grid: Array[Array[Int]], pricing: Array[Int], start: Array[Int], k: Int): List[List[Int]] =
    val toVisit = mutable.Queue((start.head, start.last, 0))
    val visited = mutable.Set((start.head, start.last))
    val items   = mutable.ListBuffer.empty[(Int, Int, Int, Int)]

    while toVisit.nonEmpty do
      val (x, y, distance) = toVisit.dequeue
      if grid(x)(y) >= pricing.head && grid(x)(y) <= pricing.last then items.append((distance, grid(x)(y), x, y))

      utils.neighbours(x, y, grid).foreach { case (nx, ny) =>
        if !visited.contains((nx, ny)) && grid(nx)(ny) != 0 then
          visited.add((nx, ny))
          toVisit.enqueue((nx, ny, distance + 1))
      }

    items.sorted.take(k).map { case (_, _, x, y) => List(x, y) }.toList
