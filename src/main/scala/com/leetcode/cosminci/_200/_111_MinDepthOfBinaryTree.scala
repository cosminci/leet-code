package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.util.chaining.*

object _111_MinDepthOfBinaryTree:

  def minDepth(root: TreeNode): Int =
    if root == null then 0
    else
      Iterator
        .iterate((Seq(root), 0)) { case (toVisit, level) =>
          toVisit.flatMap { n =>
            if n.left == null && n.right == null then return level
            else Seq(n.left, n.right).filter(_ != null)
          } -> (level + 1)
        }
        .dropWhile { case (toVisit, _) => toVisit.nonEmpty }
        .next().pipe(_ => 0)
