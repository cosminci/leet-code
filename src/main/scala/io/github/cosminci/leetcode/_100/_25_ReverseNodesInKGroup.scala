package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _25_ReverseNodesInKGroup:

  def main(args: Array[String]): Unit =
    val reversed = reverseKGroup(
      new ListNode(1, new ListNode(2)),
      2
    )
    println(reversed)

  private def reverseKGroup(head: ListNode, k: Int): ListNode =
    if head == null || head.next == null then return head

    var (finalHead, start, end) = (head, head, head)
    (1 until k).foreach { _ =>
      if end == null || end.next == null then return finalHead
      end = end.next
    }
    finalHead = end

    val nextStart      = end.next
    var curr           = start
    var prev: ListNode = null
    var next: ListNode = null

    while curr != nextStart do
      next = curr.next
      curr.next = prev
      prev = curr
      curr = next

    start.next = reverseKGroup(nextStart, k)

    finalHead
