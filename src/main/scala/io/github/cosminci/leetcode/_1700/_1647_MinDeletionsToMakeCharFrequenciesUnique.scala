package io.github.cosminci.leetcode._1700

import io.github.cosminci.utils

import scala.collection.mutable

object _1647_MinDeletionsToMakeCharFrequenciesUnique:
  def main(args: Array[String]): Unit =
    println(minDeletions("abcabc"))
    println(minDeletions("accdcdadddbaadbc"))

  private def minDeletions(s: String): Int =
    val charCounts      = utils.characterCounts(s).toArray
    var deletions       = 0
    val usedFrequencies = mutable.Set.empty[Int]

    (0 until 26).foreach { i =>
      while charCounts(i) > 0 && !usedFrequencies.add(charCounts(i)) do
        charCounts(i) = charCounts(i) - 1
        deletions += 1
    }

    deletions
