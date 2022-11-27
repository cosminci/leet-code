package com.leetcode.cosminci._2500

import com.leetcode.cosminci.utils.ListNode

object _2487_RemoveNodesFromLinkedList:

  def removeNodes(head: ListNode): ListNode =
    def dfs(curr: ListNode): ListNode =
      if curr == null then curr
      else
        curr.next = dfs(curr.next)
        if curr.next != null && curr.x < curr.next.x then curr.next else curr

    dfs(curr = head)
