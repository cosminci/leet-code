package com.leetcode.cosminci._2900

import com.leetcode.cosminci.utils.ListNode

object _2816_DoubleNumRepresentedAsLinkedList:

  def doubleIt(head: ListNode): ListNode =
    def dfs(curr: ListNode): (Int, ListNode) =
      if curr == null then (0, null)
      else
        val (carry, next) = dfs(curr.next)
        val sum           = curr.x * 2 + carry
        (sum / 10, new ListNode(sum % 10, next))

    val (carry, next) = dfs(head)
    if carry == 0 then next else new ListNode(carry, next)
