package io.github.cosminci.leetcode._100

object _49_GroupAnagrams:

  def groupAnagrams(strs: Array[String]): List[List[String]] =
    strs
      .groupBy(_.groupMapReduce(identity)(_ => 1)(_ + _))
      .values
      .map(_.toList)
      .toList
