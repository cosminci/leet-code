package com.leetcode.cosminci._600

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _501_FindModeOfBST:
  def main(args: Array[String]): Unit =
    val tree = new TreeNode(2, null, new TreeNode(2))
    println(findMode(tree).toList)

  def findMode(root: TreeNode): Array[Int] =
    val frequencies = mutable.Map.empty[Int, Int]

    def dfs(node: TreeNode): Unit =
      if node == null then return ()
      frequencies.updateWith(node.value) {
        case None    => Some(1)
        case Some(c) => Some(c + 1)
      }
      dfs(node.left)
      dfs(node.right)

    dfs(root)

    val maxFrequency = frequencies.values.max
    frequencies.view.filterKeys(k => frequencies(k) == maxFrequency).keys.toArray
