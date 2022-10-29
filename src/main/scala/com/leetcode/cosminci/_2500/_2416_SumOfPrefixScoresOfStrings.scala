package com.leetcode.cosminci._2500

import scala.collection.mutable

object _2416_SumOfPrefixScoresOfStrings:

  def sumPrefixScores(words: Array[String]): Array[Int] =
    class TrieNode(val children: mutable.Map[Char, TrieNode] = mutable.Map.empty, var freq: Int = 0)

    val trie = new TrieNode()
    words.foreach {
      _.foldLeft(trie) { (trie, char) =>
        trie.children.getOrElseUpdate(char, new TrieNode()).freq += 1
        trie.children(char)
      }
    }

    words.map {
      _.foldLeft(trie, 0) { case ((trie, score), char) =>
        (trie.children(char), score + trie.children(char).freq)
      }._2
    }
