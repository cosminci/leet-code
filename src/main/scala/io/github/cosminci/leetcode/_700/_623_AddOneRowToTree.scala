package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.collection.mutable

object _623_AddOneRowToTree {
  private def addOneRow(root: TreeNode, value: Int, depth: Int): TreeNode =
    if (depth == 1) new TreeNode(value, _left = root)
    else {
      (2 until depth).foldLeft(Seq(root)) {
        case (nodes, _) =>
          nodes.flatMap(node => Seq.empty ++ Option(node.left) ++ Option(node.right))
      }.foreach { node =>
        node.left = new TreeNode(value, _left = node.left)
        node.right = new TreeNode(value, _right = node.right)
      }
      root
    }
}
