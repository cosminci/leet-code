package io.github.cosminci.leetcode._1200

import io.github.cosminci.utils

object _1160_FindWordsThatCanBeFormedByCharacters:
  def main(args: Array[String]): Unit =
    println(countCharacters(Array("hello", "world", "leetcode"), "welldonehoneyr"))

  def countCharacters(words: Array[String], chars: String): Int =
    val availableChars = utils.characterCounts(chars)
    words.foldLeft(0) { (sum, w) =>
      if utils.characterCounts(w).zipWithIndex.forall { (count, idx) =>
          availableChars(idx) >= count
        }
      then sum + w.length
      else sum
    }
