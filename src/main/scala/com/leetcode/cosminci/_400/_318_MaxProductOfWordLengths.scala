package com.leetcode.cosminci._400

object _318_MaxProductOfWordLengths:
  def main(args: Array[String]): Unit =
    println(maxProduct(Array("abcw", "baz", "foo", "bar", "xtfn", "abcdef")))
    println(maxProduct(Array("a", "ab", "abc", "d", "cd", "bcd", "abcd")))
    println(maxProduct(Array("a", "aa", "aaa", "aaaa")))

  def maxProduct(words: Array[String]): Int =
    val bitsets = words.map(_.foldLeft(0)((bitset, char) => bitset | 1 << (char - 'a')))

    words.indices.combinations(2).foldLeft(0) { case (prevMax, Seq(i, j)) =>
      if (bitsets(i) & bitsets(j)) != 0 then prevMax
      else prevMax.max(words(i).length * words(j).length)
    }
