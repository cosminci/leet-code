package com.leetcode.cosminci._1800

import com.leetcode.cosminci.utils

import scala.collection.mutable

object _1707_MaxXORWithAnElementFromArray:

  def maximizeXor(nums: Array[Int], queries: Array[Array[Int]]): Array[Int] =
    nums.sortInPlace()
    val root   = new TrieNode()
    var numIdx = 0
    queries.zipWithIndex
      .sortInPlaceBy(_._1.last)
      .map { case (Array(queryNum, maxAllowed), originalIdx) =>
        while numIdx < nums.length && nums(numIdx) <= maxAllowed do
          addToTrie(nums(numIdx), root)
          numIdx += 1
        (findMaxXORInTrie(queryNum, root), originalIdx)
      }
      .sortBy(_._2) // sort by original index
      .map(_._1)    // get rid of the index
      .toArray

  class TrieNode(val children: mutable.Map[Int, TrieNode] = mutable.Map.empty, var value: Option[Int] = None)

  def addToTrie(n: Int, root: TrieNode): Unit =
    (31 to 0 by -1)
      .foldLeft(root) { case (node, bitIdx) =>
        val bit = (n >> bitIdx) & 1
        if node.children.contains(bit) then node.children(bit)
        else
          val newNode = new TrieNode()
          node.children.update(bit, newNode)
          newNode
      }
      .value = Some(n)

  def findMaxXORInTrie(n: Int, root: TrieNode): Int =
    if root.children.isEmpty then -1
    else
      (31 to 0 by -1)
        .foldLeft(root) { case (node, bitIdx) =>
          val bit = (n >> bitIdx) & 1
          if node.children.contains(bit ^ 1) then node.children(bit ^ 1)
          else node.children(bit)
        }
        .value
        .get ^ n
