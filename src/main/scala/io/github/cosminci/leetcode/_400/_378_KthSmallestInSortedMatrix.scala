package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _378_KthSmallestInSortedMatrix:

  def main(args: Array[String]): Unit =
    println(kthSmallest(Array(Array(1, 2, 3), Array(2, 3, 4), Array(3, 4, 5)), 9))

  def kthSmallest(matrix: Array[Array[Int]], k: Int): Int =
    if k == 1 then return matrix(0)(0)
    if matrix.length == 1 then return matrix(0)(k - 1)
    if matrix(0).length == 1 then return matrix(k - 1)(0)

    given Ordering[XYElem] = (x: XYElem, y: XYElem) => y.value.compareTo(x.value)
    val pqueue = mutable.PriorityQueue.from(matrix(0).zipWithIndex.map { case (value, col) =>
      XYElem(value, 0, col)
    })

    (1 to k)
      .foldLeft(XYElem(0, 0, 0)) { case (_, _) =>
        val min = pqueue.dequeue()
        if min.x + 1 < matrix.length then pqueue.enqueue(XYElem(matrix(min.x + 1)(min.y), min.x + 1, min.y))
        min
      }
      .value

  case class XYElem(value: Int, x: Int, y: Int)
