package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.ListNode

object _141_LinkedListCycle:

  def hasCycle(head: ListNode): Boolean =
    @annotation.tailrec
    def dfs(slow: ListNode, fast: ListNode): Boolean =
      if fast == null || fast.next == null then false
      else if fast == slow then true
      else dfs(slow.next, fast.next.next)

    if head == null then false
    else dfs(head, head.next)
