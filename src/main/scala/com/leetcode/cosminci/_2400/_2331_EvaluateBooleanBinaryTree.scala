package com.leetcode.cosminci._2400

import com.leetcode.cosminci.utils.TreeNode

object _2331_EvaluateBooleanBinaryTree:

  def evaluateTree(root: TreeNode): Boolean =
    if root.left == null then root.value == 1
    else if root.value == 2 then evaluateTree(root.left) || evaluateTree(root.right)
    else evaluateTree(root.left) && evaluateTree(root.right)
