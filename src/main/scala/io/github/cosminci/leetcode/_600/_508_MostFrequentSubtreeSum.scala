package io.github.cosminci.leetcode._600

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _508_MostFrequentSubtreeSum:
  def main(args: Array[String]): Unit =
    println(findFrequentTreeSum(new TreeNode(5, new TreeNode(2), new TreeNode(-5))).toSeq)

  def findFrequentTreeSum(root: TreeNode): Array[Int] =
    val sumCounts = mutable.Map.empty[Int, Int]
    def dfs(node: TreeNode): Int =
      if node == null then return 0
      val localSum = node.value + dfs(node.left) + dfs(node.right)
      sumCounts.update(localSum, sumCounts.getOrElse(localSum, 0) + 1)
      localSum

    dfs(root)

    val maxFrequency = sumCounts.values.max
    sumCounts.collect {
      case (sum, freq) if freq == maxFrequency => sum
    }.toArray
