package com.leetcode.cosminci._2900

import com.leetcode.cosminci.utils.{gcd, ListNode}

object _2807_InsertGcdInLinkedList:

  def insertGreatestCommonDivisors(head: ListNode): ListNode =
    def dfs(prev: ListNode, curr: ListNode): ListNode =
      if curr == null then null
      else new ListNode(gcd(prev.x, curr.x), new ListNode(curr.x, dfs(curr, curr.next)))

    new ListNode(head.x, dfs(head, head.next))
