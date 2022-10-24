package io.github.cosminci.leetcode._1300

object _1239_MaxLengthOfConcatenatedStringWithUniqueChars:

  def maxLength(arr: List[String]): Int =
    val bitsets = arr.collect {
      case s if s.distinct.length == s.length =>
        s.foldLeft(0)((bitset, char) => bitset | 1 << (char - 'a'))
    }

    @annotation.tailrec
    def bitCount(bitset: Int, cnt: Int): Int =
      if bitset == 0 then cnt else bitCount(bitset >> 1, cnt + (bitset & 1))

    def dfs(i: Int, bitset: Int): Int =
      if i == bitsets.length then bitCount(bitset, cnt = 0)
      else if (bitset & bitsets(i)) > 0 then dfs(i + 1, bitset)
      else dfs(i + 1, bitset | bitsets(i)).max(dfs(i + 1, bitset))

    dfs(i = 0, bitset = 0)
