package com.leetcode.cosminci._100

import com.leetcode.cosminci.utils.*

object _19_RemoveNthNodeFromEndOfList:

  def removeNthFromEnd(head: ListNode, n: Int): ListNode =
    def dfs(curr: ListNode, n: Int): (ListNode, Int) =
      if curr == null then (curr, 0)
      else
        val (next, count) = dfs(curr.next, n)
        curr.next = next
        if count + 1 == n then (curr.next, count + 1)
        else (curr, count + 1)

    dfs(head, n)._1
