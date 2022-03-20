package io.github.cosminci.leetcode._1100

object _1007_MinDominoRotationsForEqualRow:
  def main(args: Array[String]): Unit =
    println(minDominoRotations(Array(1, 1, 1, 1, 2, 1), Array(2, 1, 1, 1, 2, 1)))

  def minDominoRotations(tops: Array[Int], bottoms: Array[Int]): Int =
    val n = tops.length
    val s = (0 until n).foldLeft(Set.from(1 to 6))((s, i) => s.intersect(Set(tops(i), bottoms(i)))).toSeq

    if s.isEmpty then return -1

    val flips1 = (0 until n).count(i => tops(i) == s.head)
    val flips2 = (0 until n).count(i => bottoms(i) == s.head)
    (n - flips1).min(n - flips2)
