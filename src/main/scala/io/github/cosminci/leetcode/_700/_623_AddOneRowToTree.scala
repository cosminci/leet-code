package io.github.cosminci.leetcode._700

import io.github.cosminci.utils.TreeNode

import scala.util.chaining.*

object _623_AddOneRowToTree:

  def addOneRow(root: TreeNode, value: Int, depth: Int): TreeNode =
    if depth == 1 then new TreeNode(value, _left = root)
    else
      (2 until depth)
        .foldLeft(Seq(root)) { case (nodes, _) =>
          nodes.flatMap(node => Seq(Option(node.left), Option(node.right)).flatten)
        }
        .foreach { node =>
          node.left = new TreeNode(value, _left = node.left)
          node.right = new TreeNode(value, _right = node.right)
        }
        .pipe { _ => root }
