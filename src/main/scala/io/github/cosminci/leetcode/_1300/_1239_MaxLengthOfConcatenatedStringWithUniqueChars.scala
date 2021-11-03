package io.github.cosminci.leetcode._1300

object _1239_MaxLengthOfConcatenatedStringWithUniqueChars:
  def main(args: Array[String]): Unit =
    println(maxLength(List("un", "iq", "ue")))
    println(maxLength(List("cha", "r", "act", "ers")))
    println(maxLength(List("abcdefghijklmnopqrstuvwxyz")))
    println(maxLength(List("yy", "bkhwmpbiisbldzknpm")))

  def maxLength(arr: List[String]): Int =
    val bitsets = arr.collect {
      case s if (s.distinct == s) =>
        s.foldLeft(0) { case (bitset, char) =>
          bitset | 1 << (char - 'a')
        }
    }

    def bitcount(bitset: Int): Int =
      if bitset == 0 then 0
      else if (bitset & 1) == 1 then 1 + bitcount(bitset >> 1)
      else bitcount(bitset >> 1)

    def dfs(aggregateBitset: Int, idx: Int): Int =
      if idx == bitsets.length then bitcount(aggregateBitset)
      else if (aggregateBitset & bitsets(idx)) == 0 then
        math.max(dfs(aggregateBitset | bitsets(idx), idx + 1), dfs(aggregateBitset, idx + 1))
      else dfs(aggregateBitset, idx + 1)

    dfs(0, 0)
