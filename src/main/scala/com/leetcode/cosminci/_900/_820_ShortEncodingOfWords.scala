package com.leetcode.cosminci._900

import scala.collection.mutable

object _820_ShortEncodingOfWords:

  def main(args: Array[String]): Unit =
    println(minimumLengthEncodingTrie(Array("time", "me", "bell")))
    println(minimumLengthEncodingBruteForce(Array("time", "me", "bell")))

  def minimumLengthEncodingTrie(words: Array[String]): Int =
    case class TrieNode(children: mutable.Map[Char, TrieNode] = mutable.Map.empty)

    val trieRoot = words.foldLeft(TrieNode()) { (root, word) =>
      word.foldRight(root) { (char, node) =>
        node.children.getOrElseUpdate(char, TrieNode())
      }
      root
    }
    def dfs(node: TrieNode, depth: Int): Int =
      if node.children.isEmpty then depth + 1
      else node.children.values.map(dfs(_, depth + 1)).sum

    dfs(trieRoot, depth = 0)

  def minimumLengthEncodingBruteForce(words: Array[String]): Int =
    words
      .foldLeft(words.toSet)((set, w) => set.removedAll((1 until w.length).map(w.substring)))
      .toSeq
      .map(_.length + 1)
      .sum
