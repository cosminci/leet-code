package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _424_LongestRepeatingCharacterReplacement:
  def main(args: Array[String]): Unit =
    println(characterReplacement("ABAB", 2))
    println(characterReplacement("AABABBBA", 1))

  def characterReplacement(s: String, k: Int): Int =
    val counts       = mutable.Map.empty[Char, Int]
    var maxLength    = 0
    var (start, end) = (0, 0)
    while end < s.length do
      counts.updateWith(s(end)) {
        case None    => Some(1)
        case Some(v) => Some(v + 1)
      }

      var mostCommon = counts.values.max
      var currlength = end - start + 1
      while currlength - mostCommon > k do
        counts.updateWith(s(start)) {
          case None | Some(1) => None
          case Some(v)        => Some(v - 1)
        }
        currlength -= 1
        start += 1
        mostCommon = counts.values.max
      maxLength = math.max(maxLength, currlength)
      end += 1

    maxLength
