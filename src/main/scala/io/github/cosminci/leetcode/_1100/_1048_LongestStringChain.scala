package io.github.cosminci.leetcode._1100

import scala.collection.mutable

object _1048_LongestStringChain:

  def longestStrChain(words: Array[String]): Int =
    val wordsByLength = words.toSet.groupBy(_.length)
    val mem           = mutable.Map.empty[String, Int]

    def dfs(word: String): Int = mem.getOrElseUpdate(word,
      wordsByLength.get(word.length - 1) match {
        case None => 1
        case Some(shorterWords) =>
          val wildcards = word.indices.map(i => s"${word.slice(0, i)}${word.slice(i + 1, word.length)}").toSet
          shorterWords.intersect(wildcards).map(dfs).maxOption.getOrElse(0) + 1
      }
    )

    words.map(dfs).max
