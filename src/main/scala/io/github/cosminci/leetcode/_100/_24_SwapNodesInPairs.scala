package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _24_SwapNodesInPairs {
  def swapPairs(head: ListNode): ListNode = {
    if (head == null || head.next == null) return head
    val newHead = head.next
    val next = newHead.next
    newHead.next = head
    head.next = swapPairs(next)
    newHead
  }
}
