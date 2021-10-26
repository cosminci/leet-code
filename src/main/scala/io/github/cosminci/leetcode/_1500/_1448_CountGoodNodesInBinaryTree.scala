package io.github.cosminci.leetcode._1500

import io.github.cosminci.utils.TreeNode

object _1448_CountGoodNodesInBinaryTree:

  def main(args: Array[String]): Unit =
    println(goodNodes(new TreeNode(3, _left = new TreeNode(3, _left = new TreeNode(4), _right = new TreeNode(2)))))

  private def goodNodes(root: TreeNode): Int =
    def dfs(node: TreeNode, prevMax: Int): Int =
      if node == null then return 0
      val (localCount, newMax) =
        if node.value >= prevMax then (1, node.value)
        else (0, prevMax)
      localCount + dfs(node.left, newMax) + dfs(node.right, newMax)
    dfs(root, root.value)
