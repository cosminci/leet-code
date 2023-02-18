package com.leetcode.cosminci._300

import com.leetcode.cosminci.utils.TreeNode

import scala.collection.mutable

object _226_InvertBinaryTree:

  def invertTree(root: TreeNode): TreeNode =
    if root == null then null
    else new TreeNode(root.value, invertTree(root.right), invertTree(root.left))
