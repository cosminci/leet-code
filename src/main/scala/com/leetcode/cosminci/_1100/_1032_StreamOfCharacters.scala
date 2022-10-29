package com.leetcode.cosminci._1100

import scala.collection.mutable

object _1032_StreamOfCharacters:
  def main(args: Array[String]): Unit =
    val streamChecker = new StreamChecker(Array("abc", "xyz"))
    Seq('a', 'x', 'y', 'z').foreach(letter => println(streamChecker.query(letter)))

  class StreamChecker(words: Array[String]):
    class TrieNode(val children: mutable.Map[Char, TrieNode] = mutable.Map.empty, var leaf: Boolean = false)

    private val root = words.foldLeft(new TrieNode()) { (root, word) =>
      word.foldRight(root) { (char, node) =>
        node.children.getOrElseUpdate(char, new TrieNode())
      }.leaf = true
      root
    }

    private val letters = new StringBuilder()

    def query(letter: Char): Boolean =
      def dfs(node: TrieNode, idx: Int): Boolean =
        idx >= 0 && node.children.get(letters(idx)).exists(child => child.leaf || dfs(child, idx - 1))

      letters.append(letter)
      dfs(node = root, idx = letters.length() - 1)
