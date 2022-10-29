package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.TreeNode

object _2415_ReverseOddLevelsOfBinaryTree:

  def reverseOddLevels(root: TreeNode): TreeNode =
    def dfs(prevLevel: Seq[TreeNode]): Seq[Seq[Int]] =
      if prevLevel.head.left == null then Seq(prevLevel.map(_.value))
      else prevLevel.map(_.value) +: dfs(prevLevel.flatMap(n => Seq(n.left, n.right)))

    val levels = dfs(Seq(root))

    levels.indices
      .foldRight(Array.fill[TreeNode](levels.last.length * 2)(null)) { (i, children) =>
        val values = if i % 2 == 0 then levels(i) else levels(i).reverse
        values.indices.map(j => new TreeNode(values(j), children(j * 2), children(j * 2 + 1))).toArray
      }.head
