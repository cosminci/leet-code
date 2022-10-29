package com.leetcode.cosminci._500

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _437_PathSumIII:
  def main(args: Array[String]): Unit =
    println(
      pathSum(
        new TreeNode(
          10,
          new TreeNode(5, new TreeNode(3, new TreeNode(3), new TreeNode(-2)), new TreeNode(2, null, new TreeNode(1))),
          new TreeNode(-3, null, new TreeNode(11))
        ),
        8
      )
    )

  def pathSum(root: TreeNode, targetSum: Int): Int =
    def dfs(node: TreeNode, prevRollingSum: Int, prevPrefixSums: Map[Int, Int]): Int =
      if node == null then return 0
      val rollingSum = prevRollingSum + node.value
      val localCount = prevPrefixSums.getOrElse(rollingSum - targetSum, 0)
      val prefixSums = prevPrefixSums.updated(rollingSum, prevPrefixSums.getOrElse(rollingSum, 0) + 1)
      localCount + dfs(node.left, rollingSum, prefixSums) + dfs(node.right, rollingSum, prefixSums)

    dfs(root, 0, Map(0 -> 1))
