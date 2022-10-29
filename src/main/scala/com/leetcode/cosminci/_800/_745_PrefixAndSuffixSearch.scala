package com.leetcode.cosminci._800

import scala.collection.mutable

object _745_PrefixAndSuffixSearch:

  class WordFilter(val words: Array[String]):
    private class TrieNode(val next: mutable.Map[Char, TrieNode] = mutable.Map.empty, var highestIndex: Int = -1)

    private val root = words.zipWithIndex.foldLeft(new TrieNode()) { case (root, (word, i)) =>
      (0 until word.length)
        .map(i => s"${word.substring(i)}#$word")
        .foldLeft(root) { (root, wordWrap) =>
          wordWrap.foldLeft(root) { (node, char) =>
            if node.next.contains(char) then
              val nextNode = node.next(char)
              nextNode.highestIndex = i
              nextNode
            else
              val newNode = new TrieNode(highestIndex = i)
              node.next.update(char, newNode)
              newNode
          }
          root
        }
    }

    def f(prefix: String, suffix: String): Int =
      s"$suffix#$prefix"
        .foldLeft(root)((node, char) => if node.next.contains(char) then node.next(char) else return -1)
        .highestIndex
