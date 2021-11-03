package io.github.cosminci.leetcode._1000

import scala.collection.mutable

object _986_IntervalListIntersections:
  def main(args: Array[String]): Unit =
    println(
      intervalIntersection(
        Array(Array(0, 2), Array(5, 10), Array(13, 23), Array(24, 25)),
        Array(Array(1, 5), Array(8, 12), Array(15, 24), Array(25, 26))
      ).map(_.toList).toList
    )

  def intervalIntersection(firstList: Array[Array[Int]], secondList: Array[Array[Int]]): Array[Array[Int]] =
    var (idx1, idx2) = (0, 0)
    val results      = mutable.ListBuffer.empty[Array[Int]]

    while idx1 != firstList.length && idx2 != secondList.length do
      val Array(start1, end1) = firstList(idx1)
      val Array(start2, end2) = secondList(idx2)

      val candidate = Array(math.max(start1, start2), math.min(end1, end2))
      if candidate.head <= candidate.last then results.append(candidate)
      if end1 <= end2 then idx1 += 1 else idx2 += 1

    results.toArray
