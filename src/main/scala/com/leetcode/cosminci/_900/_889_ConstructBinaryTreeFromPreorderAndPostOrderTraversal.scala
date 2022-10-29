package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.TreeNode

object _889_ConstructBinaryTreeFromPreorderAndPostOrderTraversal:
  def constructFromPrePost(preorder: Array[Int], postorder: Array[Int]): TreeNode =
    def dfs(a: Int, b: Int, c: Int, d: Int): TreeNode =
      if a > b || c > d then null
      else if a == b then new TreeNode(preorder(a))
      else
        val partitionIdx = postorder.indexOf(preorder(a + 1))
        new TreeNode(
          preorder(a),
          dfs(a + 1, a + partitionIdx - c + 1, c, partitionIdx),
          dfs(a + partitionIdx - c + 2, b, partitionIdx + 1, d - 1)
        )

    dfs(0, preorder.length - 1, 0, postorder.length - 1)
