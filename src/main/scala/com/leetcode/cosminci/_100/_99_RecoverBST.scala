package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.TreeNode

object _99_RecoverBST:
  def recoverTree(root: TreeNode): Unit =
    @annotation.tailrec
    def pushLeft(stack: Seq[TreeNode], node: TreeNode): Seq[TreeNode] =
      if node == null then stack else pushLeft(stack :+ node, node.left)

    @annotation.tailrec
    def dfs(curr: TreeNode, prev: TreeNode, violations: Array[(TreeNode, TreeNode)], stack: Seq[TreeNode]): Unit =
      if curr == null && stack.isEmpty then
        val tmp = violations.head._1.value
        violations.head._1.value = violations.last._2.value
        violations.last._2.value = tmp
      else
        val newStack :+ node = pushLeft(stack, curr)
        val newViolations    = if node.value < prev.value then violations :+ (prev, node) else violations
        dfs(curr = node.right, prev = node, newViolations, newStack)

    dfs(curr = root, prev = new TreeNode(Int.MinValue), violations = Array.empty, stack = Seq.empty)
