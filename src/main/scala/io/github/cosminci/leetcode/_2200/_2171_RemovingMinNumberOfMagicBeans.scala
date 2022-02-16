package io.github.cosminci.leetcode._2200

object _2171_RemovingMinNumberOfMagicBeans:

  def minimumRemoval(beans: Array[Int]): Long =
    val sorted = beans.map(_.toLong).sorted
    sorted.sum - sorted.indices.map(i => (beans.length - i) * sorted(i)).max
