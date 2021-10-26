package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _318_MaxProductOfWordLengths:
  def main(args: Array[String]): Unit =
    println(maxProduct(Array("abcw", "baz", "foo", "bar", "xtfn", "abcdef")))
    println(maxProduct(Array("a", "ab", "abc", "d", "cd", "bcd", "abcd")))
    println(maxProduct(Array("a", "aa", "aaa", "aaaa")))

  private def maxProduct(words: Array[String]): Int =
    val bitsets = words.map { word =>
      word.foldLeft(0) {
        case (bitset, char) =>
          bitset | 1 << (char - 'a')
      }
    }
    words.indices.combinations(2).foldLeft(0) {
      case (max, Seq(i, j)) =>
        if (bitsets(i) & bitsets(j)) == 0 then
          math.max(max, words(i).length * words(j).length)
        else max
    }
