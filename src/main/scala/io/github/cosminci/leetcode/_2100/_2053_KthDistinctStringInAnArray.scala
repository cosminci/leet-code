package io.github.cosminci.leetcode._2100

object _2053_KthDistinctStringInAnArray:
  def main(args: Array[String]): Unit =
    println(kthDistinct(Array("d", "b", "c", "b", "c", "a"), 2))

  def kthDistinct(arr: Array[String], k: Int): String =
    val counts   = arr.groupBy(identity)
    val distinct = arr.filter(s => counts(s).length == 1)
    Option.when(k <= distinct.length)(distinct(k - 1)).getOrElse("")
