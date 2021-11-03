package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _472_ConcatenatedWords:
  def main(args: Array[String]): Unit =
    println(
      findAllConcatenatedWordsInADict(Array("cat", "cats", "catsdogcats", "dog", "dogcatsdog", "rat", "ratcatdogcat"))
    )

  def findAllConcatenatedWordsInADict(words: Array[String]): List[String] =
    val shorterWords = mutable.Set.empty[String]
    val results      = mutable.ListBuffer.empty[String]

    def isConcatenatable(target: String): Boolean =
      if shorterWords.isEmpty then return false
      val dp = Array.ofDim[Boolean](target.length + 1)
      dp(target.length) = true

      (target.length - 1 to 0 by -1).foreach { i =>
        dp(i) = shorterWords.exists { w =>
          target.substring(i).startsWith(w) && dp(i + w.length)
        }
      }
      dp.head

    words.sortBy(_.length).foreach { word =>
      if isConcatenatable(word) then results.append(word)
      shorterWords.add(word)
    }

    results.toList
