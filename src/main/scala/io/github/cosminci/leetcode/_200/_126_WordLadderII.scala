package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _126_WordLadderII:

  def main(args: Array[String]): Unit =
    println(findLadders("red", "tax", List("ted", "tex", "red", "tax", "tad", "den", "rex", "pee")))

  private def findLadders(beginWord: String, endWord: String, wordList: List[String]): List[List[String]] =
    if !wordList.contains(endWord) then return List.empty

    val (wordToWildcards, wildcardToWords) = wildcardMappings(wordList :+ beginWord)

    val toVisit = mutable.Queue.empty[Vector[String]]
    val visited = mutable.Map.empty[String, Int]
    toVisit.enqueue(Vector(beginWord))
    visited.addOne((beginWord, 1))

    var foundLength = 0
    val paths       = mutable.ListBuffer.empty[List[String]]
    while toVisit.nonEmpty do
      val wordPath = toVisit.dequeue()
      if foundLength > 0 && wordPath.length > foundLength then return paths.toList
      if wordPath.last == endWord then
        foundLength = wordPath.length
        paths.addOne(wordPath.toList)
      else
        wordToWildcards(wordPath.last).flatMap(wildcardToWords).foreach { w =>
          val newLength = wordPath.length + 1
          if !visited.contains(w) || visited(w) == newLength then
            toVisit.enqueue(wordPath :+ w)
            visited.addOne((w, newLength))
        }

    paths.toList


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