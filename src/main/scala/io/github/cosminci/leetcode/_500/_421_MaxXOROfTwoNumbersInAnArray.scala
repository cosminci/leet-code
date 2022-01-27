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
      .foldLeft(0, 0) { case ((prevMaxXOR, prevMask), i) =>
        val newMask   = prevMask | (1 << i)
        val leftParts = Set.from(nums.map(_ & newMask))
        val candidate = prevMaxXOR | (1 << i)
        val newMaxXOR =
          if leftParts.exists(a => leftParts.contains(candidate ^ a)) then candidate
          else prevMaxXOR

        (newMaxXOR, newMask)
      }
      ._1

  def findMaximumXORTrie(nums: Array[Int]): Int =
    val root = buildTrie(nums)
    nums.map { n =>
      (31 to 0 by -1)
        .foldLeft(root) { (node, bitIdx) =>
          val bit = (n >> bitIdx) & 1
          node.children.getOrElse(bit ^ 1, node.children(bit))
        }
        .value ^ n
    }.max

  class TrieNode(val children: mutable.Map[Int, TrieNode] = mutable.Map.empty, var value: Int = 0)

  private def buildTrie(nums: Array[Int]): TrieNode =
    nums.foldLeft(new TrieNode()) { (root, n) =>
      (31 to 0 by -1)
        .foldLeft(root) { (node, bitIdx) =>
          val bit = (n >> bitIdx) & 1
          node.children.getOrElseUpdate(bit, new TrieNode())
        }
        .value = n
      root
    }
