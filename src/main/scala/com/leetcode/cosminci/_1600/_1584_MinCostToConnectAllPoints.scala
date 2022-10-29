package com.leetcode.cosminci._1600

import scala.collection.mutable

object _1584_MinCostToConnectAllPoints:

  def main(args: Array[String]): Unit =
    val adjMatrix = Array(Array(0, 0), Array(2, 2), Array(3, 10), Array(5, 2), Array(7, 0))
    println(minCostConnectPointsPrimI(adjMatrix))
    println(minCostConnectPointsPrimII(adjMatrix))

  def minCostConnectPointsPrimI(points: Array[Array[Int]]): Int =
    val pq      = mutable.PriorityQueue.empty[(Int, Int)]
    val visited = Array.ofDim[Boolean](points.length)

    Iterator
      .iterate((0, 0, 1)) { case (totalCost, i, connected) =>
        visited(i) = true
        points.indices
          .filter(p => !visited(p))
          .foreach(j => pq.enqueue((-distance(points, i, j), j)))
        val (d, j) = Iterator
          .iterate((0, i))(_ => pq.dequeue())
          .dropWhile { case (_, j) => visited(j) }
          .next()
        (totalCost - d, j, connected + 1)
      }
      .dropWhile { case (_, _, connected) => connected < points.length }
      .next()
      ._1

  def minCostConnectPointsPrimII(points: Array[Array[Int]]): Int =
    Iterator
      .iterate((0, 0, 1, Seq.fill(points.length)(10_000_000))) { case (totalCost, i, connected, minDistance) =>
        val newMinDistance = points.indices.map { j =>
          if i == j || minDistance(j) == Int.MaxValue then Int.MaxValue
          else minDistance(j).min(distance(points, i, j))
        }
        val closest = points.indices.minBy(idx => newMinDistance(idx))
        (totalCost + newMinDistance(closest), closest, connected + 1, newMinDistance)
      }
      .dropWhile { case (_, _, connected, _) => connected < points.length }
      .next()
      ._1

  private def distance(points: Array[Array[Int]], i: Int, j: Int) =
    val (Array(x0, y0), Array(x1, y1)) = (points(i), points(j))
    (x0 - x1).abs + (y0 - y1).abs
