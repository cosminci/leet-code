package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.*

object _109_ConvertSortedListToBST:

  def sortedListToBST(head: ListNode): TreeNode =
    val nums = linkedListToSeq(head)

    def dfs(i: Int, j: Int): TreeNode =
      if i > j then null
      else if i == j then new TreeNode(nums(i))
      else
        val mid = (i + j) / 2
        new TreeNode(nums(mid), dfs(i, mid - 1), dfs(mid + 1, j))

    dfs(i = 0, j = nums.length - 1)
