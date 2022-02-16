package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _24_SwapNodesInPairs:

  def swapPairs(head: ListNode): ListNode =
    if head == null || head.next == null then head
    else new ListNode(head.next.x, new ListNode(head.x, swapPairs(head.next.next)))
