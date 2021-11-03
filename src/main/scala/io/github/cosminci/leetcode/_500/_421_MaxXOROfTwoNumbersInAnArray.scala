package io.github.cosminci.leetcode._500

import scala.collection.mutable

object _421_MaxXOROfTwoNumbersInAnArray:
  def main(args: Array[String]): Unit =
    println(findMaximumXORBitManipulation(Array(8, 4, 3)))
    println(findMaximumXORTrie(Array(8, 4, 3)))
    println(findMaximumXORBitManipulation(Array(3, 10, 5, 25, 2, 8)))
    println(findMaximumXORTrie(Array(3, 10, 5, 25, 2, 8)))

  def findMaximumXORBitManipulation(nums: Array[Int]): Int =
    (31 to 0 by -1)
      .foldLeft(0, 0) { case ((prevMaxXOR, mask), i) =>
        val newMask   = mask | (1 << i)
        val leftParts = Set.from(nums.map(_ & newMask))
        val candidate = prevMaxXOR | (1 << i)
        val newMaxXOR = leftParts
          .find(leftPart => leftParts.contains(candidate ^ leftPart))
          .map(_ => candidate)
          .getOrElse(prevMaxXOR)
        (newMaxXOR, newMask)
      }
      ._1

  class TrieNode(val children: mutable.Map[Int, TrieNode] = mutable.Map.empty, var value: Option[Int] = None)

  def buildTrie(nums: Array[Int]): TrieNode = {
    val root = new TrieNode()
    nums.foreach { n =>
      var node = root
      (31 to 0 by -1).foreach { bitIdx =>
        val bit = (n >> bitIdx) & 1
        if (node.children.contains(bit)) {
          node = node.children(bit)
        } else {
          val newNode = new TrieNode()
          node.children.update(bit, newNode)
          node = newNode
        }
      }
      node.value = Some(n)
    }
    root
  }

  def findMaximumXORTrie(nums: Array[Int]): Int = {
    val root = buildTrie(nums)
    nums.map { n =>
      var node = root
      (31 to 0 by -1).foreach { bitIdx =>
        val bit = (n >> bitIdx) & 1
        if (node.children.contains(bit ^ 1))
          node = node.children(bit ^ 1)
        else
          node = node.children(bit)
      }
      node.value.get ^ n
    }.max
  }
