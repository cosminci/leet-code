package io.github.cosminci.leetcode._700

import scala.collection.mutable

object _648_ReplaceWords:
  private def replaceWords(dictionary: List[String], sentence: String): String =
    class TrieNode(
        val children: mutable.Map[Char, TrieNode] = mutable.Map.empty,
        var isLeaf: Boolean = false
    )

    val trieRoot =
      val root = new TrieNode()
      dictionary.sortBy(_.length).foreach { word =>
        word
          .foldLeft(root) { case (node, char) =>
            node.children.getOrElseUpdate(char, new TrieNode())
          }
          .isLeaf = true
      }
      root

    def findRoot(word: String): Option[String] =
      word.indices.foldLeft(trieRoot) { case (node, i) =>
        if node.isLeaf then return Some(word.substring(0, i))
        if !node.children.contains(word(i)) then return None
        node.children(word(i))
      }
      None

    sentence.split(' ').map(w => findRoot(w).getOrElse(w)).mkString(" ")
