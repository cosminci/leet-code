package com.leetcode.cosminci._3100

import scala.collection.mutable
import scala.util.chaining.*

object _3043_FindLengthOfLongestCommonPrefix:

  def main(args: Array[String]): Unit =
    println(longestCommonPrefix(Array(12345), Array(15)))

  def longestCommonPrefix(arr1: Array[Int], arr2: Array[Int]): Int =
    val trieRoot = arr1.foldLeft(new TrieNode) { (root, num) =>
      num.toString
        .foldLeft(root) { (node, digit) => node.children.getOrElseUpdate(digit, new TrieNode) }
        .pipe(_ => root)
    }
    def checkPrefix(num: String) =
      Iterator
        .iterate((trieRoot, 0)) { case (node, i) => (node.children(num(i)), i + 1) }
        .dropWhile { case (node, i) => i < num.length && node.children.contains(num(i)) }.next()
        .pipe { case (_, i) => i }

    arr2.map(num => checkPrefix(num.toString)).max

  private class TrieNode:
    val children = mutable.Map.empty[Char, TrieNode]
