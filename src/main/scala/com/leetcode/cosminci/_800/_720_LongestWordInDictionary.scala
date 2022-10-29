package com.leetcode.cosminci._800

import scala.collection.immutable.TreeMap

object _720_LongestWordInDictionary:
  def main(args: Array[String]): Unit =
    println(longestWord(Array("w", "wo", "wor", "worl", "world")))
    println(longestWord(Array("a", "banana", "app", "appl", "ap", "apply", "apple")))

  def longestWord(words: Array[String]): String =
    val prefixGroupsByLength = words.groupBy(_.length).toSeq.sortBy(_._1).map(_._2)

    var prevPrefixes = Set("")
    prefixGroupsByLength.foreach { newPrefixes =>
      val nextPrefixes = newPrefixes.filter(p => prevPrefixes.contains(p.dropRight(1)))
      if nextPrefixes.isEmpty then return prevPrefixes.toSeq.sorted.head
      else
        prevPrefixes = nextPrefixes.toSet
        nextPrefixes
    }
    prevPrefixes.toSeq.sorted.head
