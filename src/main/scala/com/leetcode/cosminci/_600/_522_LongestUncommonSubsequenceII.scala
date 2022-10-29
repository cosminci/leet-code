package com.leetcode.cosminci._600

object _522_LongestUncommonSubsequenceII:
  def main(args: Array[String]): Unit =
    println(findLUSlength(Array("abce", "abcd")))

  def findLUSlength(strs: Array[String]): Int =
    strs.sortInPlaceBy(-_.length)

    strs.indices
      .find { i =>
        strs.indices.forall { j =>
          if i == j then true else !isSubsequence(strs(i), strs(j))
        }
      }
      .map(strs(_).length)
      .getOrElse(-1)

  def isSubsequence(target: String, source: String): Boolean =
    if target.length > source.length then return false
    var (i, j) = (0, 0)
    while i < source.length && j < target.length do
      if source(i) == target(j) then j += 1
      i += 1
    j == target.length
