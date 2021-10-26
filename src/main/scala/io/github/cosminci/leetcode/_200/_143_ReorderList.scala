package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _143_ReorderList:

  def main(args: Array[String]): Unit =
    reorderList(new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4, new ListNode(5))))))

  private def reorderList(head: ListNode): Unit =
    var length  = 1
    var tracker = head
    while tracker.next != null do
      length += 1
      tracker = tracker.next
    if length <= 2 then return

    tracker = head
    var prev: ListNode = null
    (1 to (length / 2 + length % 2)).foreach { _ =>
      prev = tracker
      tracker = tracker.next
    }
    prev.next = null

    prev = null
    while tracker != null do
      val next = tracker.next
      tracker.next = prev
      prev = tracker
      tracker = next

    var start = head
    var end   = prev
    while start != null && end != null do
      val startNext = start.next
      start.next = end
      val endNext = end.next
      end.next = startNext
      start = startNext
      end = endNext
