package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _820_ShortEncodingOfWords:

  def main(args: Array[String]): Unit =
    println(minimumLengthEncodingTrie(Array("time", "me", "bell")))
    println(minimumLengthEncodingBruteForce(Array("time", "me", "bell")))

  private def minimumLengthEncodingTrie(words: Array[String]): Int =
    class TrieNode(val children: mutable.Map[Char, TrieNode] = mutable.Map.empty)

    val trieRoot = new TrieNode()
    words.foreach { word =>
      word.reverse.foldLeft(trieRoot) { (node, char) =>
        node.children.getOrElseUpdate(char, new TrieNode())
      }
    }
    def dfs(node: TrieNode, depth: Int): Int =
      if node.children.isEmpty then depth + 1
      else node.children.values.map(dfs(_, depth + 1)).sum

    dfs(trieRoot, 0)

  private def minimumLengthEncodingBruteForce(words: Array[String]): Int =
    words
      .foldLeft(words.toSet) { (set, w) =>
        set.removedAll((1 until w.length).map(w.substring))
      }
      .toSeq
      .map(_.length + 1)
      .sum
