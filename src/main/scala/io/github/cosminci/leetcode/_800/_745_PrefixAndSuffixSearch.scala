package io.github.cosminci.leetcode._800

import scala.collection.mutable

object _745_PrefixAndSuffixSearch:

  def main(args: Array[String]): Unit =
    val wordFilter = new WordFilter(
      Array(
        "cabaabaaaa",
        "ccbcababac",
        "bacaabccba",
        "bcbbcbacaa",
        "abcaccbcaa",
        "accabaccaa",
        "cabcbbbcca",
        "ababccabcb",
        "caccbbcbab",
        "bccbacbcba"
      )
    )
    println(wordFilter.f("bccbacbcba", "a"))
    println(wordFilter.f("ab", "abcaccbcaa"))
    println(wordFilter.f("a", "aa"))
    println(wordFilter.f("cabaaba", "abaaaa"))
    println(wordFilter.f("cacc", "accbbcbab"))

  class WordFilter(val words: Array[String]):
    private class TrieNode(val next: mutable.Map[Char, TrieNode], var highestIndex: Int)

    private val root = new TrieNode(mutable.Map.empty, highestIndex = -1)

    words.indices.foreach { i =>
      val word = words(i)
      (0 until word.length)
        .map { j =>
          val (fh, sh) = word.splitAt(j)
          s"$sh#$word"
        }
        .foreach { w =>
          w.foldLeft(root) { case (node, char) =>
            if node.next.contains(char) then
              val next = node.next(char)
              next.highestIndex = i
              next
            else
              val newNode = new TrieNode(mutable.Map.empty, highestIndex = i)
              node.next.update(char, newNode)
              newNode
          }
        }
    }

    def f(prefix: String, suffix: String): Int =
      val target = s"$suffix#$prefix"
      target
        .foldLeft(root) { case (node, char) =>
          if node.next.contains(char) then node.next(char)
          else return -1
        }
        .highestIndex
