package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _127_WordLadder:

  def main(args: Array[String]): Unit =
    println(ladderLength("hit", "cog", List("hot", "dot", "dog", "lot", "log", "cog")))

  def ladderLength(beginWord: String, endWord: String, wordList: List[String]): Int =
    val (wordToWildcards, wildcardToWords) = wildcardMappings(wordList :+ beginWord)

    val toVisit = mutable.Queue((beginWord, 1))
    val visited = mutable.Set(beginWord)

    while toVisit.nonEmpty do
      val (word, changeCount) = toVisit.dequeue()
      if word == endWord then return changeCount

      wordToWildcards(word)
        .flatMap(wildcardToWords)
        .filter(w => !visited.contains(w))
        .foreach { w =>
          toVisit.enqueue((w, changeCount + 1))
          visited.addOne(w)
        }
    0

  private def wildcardMappings(words: List[String]) =
    words.foldLeft(Map.empty[String, Seq[String]], Map.empty[String, Seq[String]].withDefaultValue(Seq.empty)) {
      case ((wordToWildcards, wildcardToWords), word) =>
        val wildcards = word.indices.map(word.updated(_, '*'))

        val newWcToWords = wildcards.foldLeft(wildcardToWords) { (wildcardToWords, wildcard) =>
          wildcardToWords.updated(
            wildcard,
            wildcardToWords.get(wildcard) match
              case None        => Seq(word)
              case Some(words) => words :+ word
          )
        }

        (wordToWildcards.updated(word, wildcards), newWcToWords)
    }
