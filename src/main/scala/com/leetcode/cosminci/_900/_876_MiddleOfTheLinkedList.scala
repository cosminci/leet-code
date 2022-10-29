package com.leetcode.cosminci._900

import com.leetcode.cosminci.utils.ListNode

object _876_MiddleOfTheLinkedList:
  def middleNode(head: ListNode): ListNode =
    @annotation.tailrec
    def dfs(slow: ListNode, fast: ListNode): ListNode =
      if fast == null || fast.next == null then slow
      else dfs(slow.next, fast.next.next)

    dfs(head, head)
