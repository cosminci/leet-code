package com.leetcode.cosminci._700

import com.leetcode.cosminci.utils.TreeNode

object _606_ConstructStringFromBinaryTree:

  def tree2str(root: TreeNode): String =
    if root == null then ""
    else
      val left = Option(root.left).map(c => s"(${tree2str(c)})").orElse(Option(root.right).map(_ => "()")).getOrElse("")
      val right = Option(root.right).map(tree2str).map(s => s"($s)").getOrElse("")
      s"${root.value}$left$right"
