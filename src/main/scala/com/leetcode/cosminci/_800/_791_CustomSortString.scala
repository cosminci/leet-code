package com.leetcode.cosminci._800

object _791_CustomSortString:
  def main(args: Array[String]): Unit =
    println(customSortString("cba", "abcd"))

  def customSortString(order: String, str: String): String =
    val orderMap = Map.from(order.zipWithIndex)
    str.sortBy(c => orderMap.getOrElse(c, Int.MaxValue))
