package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _127_WordLadder:

  def main(args: Array[String]): Unit =
    println(ladderLength("hit", "cog", List("hot", "dot", "dog", "lot", "log", "cog")))

  private def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int =
    if !wordList.contains(endWord) then return 0

    val (wordToWildcards, wildcardToWords) = wildcardMappings(wordList :+ beginWord)

    val toVisit = mutable.Queue.empty[(String, Int)]
    val visited = mutable.Set.empty[String]
    toVisit.enqueue((beginWord, 1))
    visited.addOne(beginWord)

    while toVisit.nonEmpty do
      val (word, changeCount) = toVisit.dequeue()
      if word == endWord then return changeCount
      else
        wordToWildcards(word).flatMap(wildcardToWords).foreach { w =>
          if !visited.contains(w) then
            toVisit.enqueue((w, changeCount + 1))
            visited.addOne(w)
        }
    0

  private def wildcardMappings(dictionary: List[String]) =
    val wordToWildcards = mutable.Map.empty[String, Seq[String]]
    val wildcardToWords = mutable.Map.empty[String, Seq[String]]

    dictionary.foreach { word =>
      wordToWildcards.update(word, word.indices.map(i => word.updated(i, '*')))
      wordToWildcards(word).foreach { wildcard =>
        wildcardToWords.updateWith(wildcard) {
          case None        => Some(Seq(word))
          case Some(words) => Some(words :+ word)
        }
      }
    }

    (wordToWildcards.toMap, wildcardToWords.toMap)
