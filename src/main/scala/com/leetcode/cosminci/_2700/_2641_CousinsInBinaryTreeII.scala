package com.leetcode.cosminci._2700

import com.leetcode.cosminci.utils.TreeNode

object _2641_CousinsInBinaryTreeII:

  def replaceValueInTree(root: TreeNode): TreeNode =
    @annotation.tailrec
    def dfs1(nodes: Seq[TreeNode], sums: Seq[Int]): Seq[Int] =
      if nodes.isEmpty then sums
      else dfs1(nodes.flatMap(n => Seq(Option(n.left), Option(n.right)).flatten), sums :+ nodes.map(_.value).sum)

    val levelSums = dfs1(nodes = Seq(root), sums = Seq.empty)

    def dfs2(node: TreeNode, sibling: TreeNode, level: Int): TreeNode =
      if node == null then null
      else if level <= 1 then
        new TreeNode(0, dfs2(node.left, node.right, level + 1), dfs2(node.right, node.left, level + 1))
      else
        new TreeNode(
          levelSums(level) - node.value - Option(sibling).map(_.value).getOrElse(0),
          dfs2(node.left, sibling = node.right, level + 1),
          dfs2(node.right, sibling = node.left, level + 1)
        )

    dfs2(root, sibling = null, level = 0)
