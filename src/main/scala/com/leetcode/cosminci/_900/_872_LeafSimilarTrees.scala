package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.TreeNode

object _872_LeafSimilarTrees:

  def leafSimilar(root1: TreeNode, root2: TreeNode): Boolean =
    leaves(root1) == leaves(root2)

  private def leaves(node: TreeNode): Seq[Int] =
    if node.left == null && node.right == null then Seq(node.value)
    else Seq(Option(node.left), Option(node.right)).flatten.flatMap(leaves)
