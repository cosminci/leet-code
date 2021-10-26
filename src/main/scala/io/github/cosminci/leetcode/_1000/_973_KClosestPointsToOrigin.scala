package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _973_KClosestPointsToOrigin:

  def main(args: Array[String]): Unit =
    println(kClosestHeap(Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2).map(_.toList).toList)
    println(kClosestSort(Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2).map(_.toList).toList)

  given Ordering[Array[Int]] = (x: Array[Int], y: Array[Int]) =>
    val i  = y.head * y.head + y.last * y.last
    val i1 = x.head * x.head + x.last * x.last
    i.compare(i1)

  private def kClosestHeap(points: Array[Array[Int]], k: Int): Array[Array[Int]] =
    val pqueue = mutable.PriorityQueue.empty[Array[Int]]
    val result = Array.ofDim[Array[Int]](k)
    pqueue.addAll(points)
    (0 until k).foreach { i =>
      result(i) = pqueue.dequeue()
    }
    result

  private def kClosestSort(points: Array[Array[Int]], k: Int): Array[Array[Int]] =
    points
      .sortBy { case Array(x, y) =>
        x * x + y * y
      }
      .take(k)
