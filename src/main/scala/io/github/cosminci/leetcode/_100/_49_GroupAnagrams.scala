package io.github.cosminci.leetcode._100

import io.github.cosminci.utils

object _49_GroupAnagrams:
  def main(args: Array[String]): Unit =
    println(groupAnagrams(Array("eat", "tea", "tan", "ate", "nat", "bat")))

  def groupAnagrams(strs: Array[String]): List[List[String]] =
    strs.groupBy(s => utils.characterCounts(s)).values.map(_.toList).toList
