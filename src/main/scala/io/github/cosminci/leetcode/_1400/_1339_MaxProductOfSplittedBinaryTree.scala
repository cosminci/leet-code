package io.github.cosminci.leetcode._1400

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _1339_MaxProductOfSplittedBinaryTree:
  def main(args: Array[String]): Unit =
    println(maxProduct(new TreeNode(1, new TreeNode(1))))

  private def maxProduct(root: TreeNode): Int =
    val sums = mutable.Map.empty[TreeNode, BigInt]
    def dfsSum(node: TreeNode): BigInt =
      if node == null then return BigInt(0)
      val leftSum  = dfsSum(node.left)
      val rightSum = dfsSum(node.right)
      val total    = BigInt(node.value) + leftSum + rightSum
      sums.update(node, total)
      total

    def dfsMax(node: TreeNode): BigInt =
      if node == null then return BigInt(0)
      (sums(node) * (sums(root) - sums(node)))
        .max(dfsMax(node.left).max(dfsMax(node.right)))

    dfsSum(root)
    (dfsMax(root) % 1_000_000_007).toInt
