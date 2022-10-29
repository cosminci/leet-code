package com.leetcode.cosminci._1000

object _986_IntervalListIntersections:
  def main(args: Array[String]): Unit =
    println(
      intervalIntersection(
        Array(Array(0, 2), Array(5, 10), Array(13, 23), Array(24, 25)),
        Array(Array(1, 5), Array(8, 12), Array(15, 24), Array(25, 26))
      ).map(_.toList).toList
    )

  def intervalIntersection(firstList: Array[Array[Int]], secondList: Array[Array[Int]]): Array[Array[Int]] =
    @annotation.tailrec
    def dfs(idx1: Int, idx2: Int, result: Array[Array[Int]]): Array[Array[Int]] =
      if idx1 == firstList.length || idx2 == secondList.length then result
      else
        val Array(start1, end1) = firstList(idx1)
        val Array(start2, end2) = secondList(idx2)

        val candidate      = Array(math.max(start1, start2), math.min(end1, end2))
        val (nidx1, nidx2) = Option.when(end1 <= end2)(idx1 + 1, idx2).getOrElse(idx1, idx2 + 1)
        dfs(nidx1, nidx2, result ++ Option.when(candidate.head <= candidate.last)(candidate))

    dfs(idx1 = 0, idx2 = 0, Array.empty)
