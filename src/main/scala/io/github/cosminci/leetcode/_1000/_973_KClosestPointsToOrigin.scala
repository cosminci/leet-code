package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _973_KClosestPointsToOrigin:

  def main(args: Array[String]): Unit =
    println(kClosestHeap(Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2).map(_.toList).toList)
    println(kClosestSort(Array(Array(3, 3), Array(5, -1), Array(-2, 4)), 2).map(_.toList).toList)

  def kClosestHeap(points: Array[Array[Int]], k: Int): Array[Array[Int]] =
    val pqueue = mutable.PriorityQueue.empty[Array[Int]](Ordering.by(distanceToOrigin).reverse)
    val result = Array.ofDim[Array[Int]](k)
    pqueue.addAll(points)
    (0 until k).foreach { i =>
      result(i) = pqueue.dequeue()
    }
    result

  def kClosestSort(points: Array[Array[Int]], k: Int): Array[Array[Int]] = 
    points.sortBy(distanceToOrigin).take(k)


  private def distanceToOrigin(p: Array[Int]) = p.head * p.head + p.last * p.last