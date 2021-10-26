package io.github.cosminci.leetcode._1900

object _1888_MinNumberOfFlipsToMakeBinaryStringAlternating:
  def main(args: Array[String]): Unit =
    println(minFlips("010"))

  private def minFlips(input: String): Int =
    val n        = input.length
    val (t1, t2) = ("01" * n, "10" * n)
    val s        = input + input

    val indices = input.indices
    var t1Diff  = indices.count(i => input(i) != t1(i))
    var t2Diff  = indices.count(i => input(i) != t2(i))
    var min     = math.min(t1Diff, t2Diff)
    (1 until n).foreach { i =>
      if s(i - 1) != t1(i - 1) then t1Diff -= 1
      if s(n + i - 1) != t1(n + i - 1) then t1Diff += 1
      if s(i - 1) != t2(i - 1) then t2Diff -= 1
      if s(n + i - 1) != t2(n + i - 1) then t2Diff += 1
      min = math.min(min, math.min(t1Diff, t2Diff))
    }

    min
