package io.github.cosminci.leetcode._2400

object _2375_ConstructSmallestNumberFromDIString:

  def smallestNumber(pattern: String): String =
    (0 to pattern.length)
      .foldLeft(Array.empty[Int], Array.empty[Int]) { case ((result, buffer), i) =>
        if i < pattern.length && pattern(i) == 'D' then (result, buffer :+ (i + 1))
        else ((result :+ (i + 1)) ++ buffer.reverse, Array.empty)
      }._1.mkString
