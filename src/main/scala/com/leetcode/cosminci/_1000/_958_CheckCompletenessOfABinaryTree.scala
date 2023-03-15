package com.leetcode.cosminci._1000

import com.leetcode.cosminci.utils.TreeNode

object _958_CheckCompletenessOfABinaryTree:

  def isCompleteTree(root: TreeNode): Boolean =
    @annotation.tailrec
    def dfs(toVisit: Seq[TreeNode], foundNull: Boolean): Boolean =
      toVisit match
        case Nil => true
        case head +: tail =>
          if head == null then dfs(tail, foundNull = true)
          else if foundNull then false
          else dfs(tail ++ Seq(head.left, head.right), foundNull)

    dfs(Seq(root), foundNull = false)
