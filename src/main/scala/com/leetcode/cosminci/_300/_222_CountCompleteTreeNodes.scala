package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils.TreeNode

import scala.util.chaining.*

object _222_CountCompleteTreeNodes:

  def countNodes(node: TreeNode): Int =
    if node == null then 0
    else
      Iterator
        .iterate((node, node, 0)) { case (l, r, h) => (l.left, r.right, h + 1) }
        .dropWhile { case (_, r, _) => r != null }
        .next()
        .pipe { case (l, _, h) =>
          if l == null then (1 << h) - 1
          else 1 + countNodes(node.left) + countNodes(node.right)
        }
