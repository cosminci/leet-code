package com.leetcode.cosminci._2200

import com.leetcode.cosminci.utils.ListNode

object _2181_MergeNodesInBetweenZeros:

  def mergeNodes(head: ListNode): ListNode =
    def dfs(curr: ListNode, sum: Int): ListNode =
      if curr == null then null
      else if curr.x > 0 then dfs(curr.next, sum + curr.x)
      else new ListNode(sum, dfs(curr.next, sum = 0))

    dfs(head.next, sum = 0)
