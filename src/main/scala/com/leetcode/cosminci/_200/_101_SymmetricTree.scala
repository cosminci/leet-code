package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _101_SymmetricTree:
  def isSymmetric(root: TreeNode): Boolean =
    def dfs(l: TreeNode, r: TreeNode): Boolean =
      if l == null ^ r == null then false
      else if l == null then true
      else l.value == r.value && dfs(l.left, r.right) && dfs(l.right, r.left)

    dfs(root.left, root.right)
