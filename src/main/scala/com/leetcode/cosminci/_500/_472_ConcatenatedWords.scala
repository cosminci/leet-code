package com.leetcode.cosminci._500

object _472_ConcatenatedWords:

  def findAllConcatenatedWordsInADict(words: Array[String]): List[String] =
    def isConcatenatable(shorterWords: Set[String], target: String): Boolean =
      shorterWords.nonEmpty &&
        (1 to target.length)
          .foldLeft(Seq(true)) { (dp, i) =>
            dp :+ (0 until i).exists(j => dp(j) && shorterWords.contains(target.substring(j, i)))
          }.last

    words.sortBy(_.length)
      .foldLeft(Seq.empty[String], Set.empty[String]) { case ((results, shorterWords), word) =>
        val newResults = if isConcatenatable(shorterWords, word) then results :+ word else results
        (newResults, shorterWords + word)
      }._1.toList
