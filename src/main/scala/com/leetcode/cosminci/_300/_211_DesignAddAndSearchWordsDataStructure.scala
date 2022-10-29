package com.leetcode.cosminci._300

import scala.collection.mutable

object _211_DesignAddAndSearchWordsDataStructure:

  class WordDictionary():
    private val prefixTreeRoot = new Node(children = mutable.Map.empty)

    def addWord(word: String): Unit =
      word.foldLeft(prefixTreeRoot) { (node, char) =>
        node.children.getOrElseUpdate(char, new Node())
      }.isLeaf = true

    def search(word: String): Boolean =
      def dfs(node: Node, idx: Int): Boolean =
        if idx == word.length then node.isLeaf
        else if word(idx) == '.' then node.children.values.exists(dfs(_, idx + 1))
        else node.children.get(word(idx)) match
          case None           => false
          case Some(nextNode) => dfs(nextNode, idx + 1)

      dfs(prefixTreeRoot, idx = 0)

    class Node(val children: mutable.Map[Char, Node] = mutable.Map.empty, var isLeaf: Boolean = false)
