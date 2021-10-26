package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _211_DesignAddAndSearchWordsDataStructure:
  def main(args: Array[String]): Unit =
    val dict = new WordDictionary()
    dict.addWord("ba")
    dict.addWord("dad")
    dict.addWord("bax")
    dict.addWord("bbb")
    println(dict.search("ba"))
    println(dict.search("b.b"))
    println(dict.search(".aq"))

  class WordDictionary():
    private val prefixTreeRoot = Node(children = mutable.Map.empty, isLeaf = false)

    def addWord(word: String) =
      var node = prefixTreeRoot
      word.toCharArray.zipWithIndex.foreach { (char, idx) =>
        if !node.children.contains(char) then
          node.children.update(char, Node(children = mutable.Map.empty, isLeaf = idx == word.length - 1))
        node = node.children(char)
      }

    def search(word: String): Boolean =
      def dfs(node: Node, wordIndex: Int): Boolean =
        if wordIndex == word.length then return node.isLeaf
        if word(wordIndex) == '.' then node.children.exists(n => dfs(n._2, wordIndex + 1))
        else if node.children.contains(word(wordIndex)) then dfs(node.children(word(wordIndex)), wordIndex + 1)
        else false

      dfs(prefixTreeRoot, 0)

    case class Node(children: mutable.Map[Char, Node], isLeaf: Boolean)
