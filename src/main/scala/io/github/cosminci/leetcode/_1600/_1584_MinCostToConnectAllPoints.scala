package io.github.cosminci.leetcode._1600

import scala.collection.mutable

object _1584_MinCostToConnectAllPoints:

  def main(args: Array[String]): Unit =
    println(minCostConnectPointsPrim(Array(Array(0, 0), Array(2, 2), Array(3, 10), Array(5, 2), Array(7, 0))))

  def minCostConnectPointsPrim(points: Array[Array[Int]]): Int =
    val pq      = mutable.PriorityQueue.empty[(Int, Int)]
    val visited = Array.ofDim[Boolean](points.length)

    def enqueueNewPoints(i: Int): Unit =
      points.indices
        .filter(n => !visited(n))
        .foreach { j =>
          val (Array(x0, y0), Array(x1, y1)) = (points(i), points(j))
          val distance                       = (x0 - x1).abs + (y0 - y1).abs
          pq.enqueue((-distance, j))
        }

    def nextCheapestPoint(curr: Int) =
      Iterator
        .iterate((0, curr))(_ => pq.dequeue())
        .dropWhile { case (_, j) => visited(j) }
        .next()

    Iterator
      .iterate((0, 0, 1)) { case (totalCost, i, connected) =>
        visited(i) = true
        enqueueNewPoints(i)
        val (distance, j) = nextCheapestPoint(i)
        (totalCost - distance, j, connected + 1)
      }
      .dropWhile { case (_, _, connected) => connected < points.length }
      .next()
      ._1
