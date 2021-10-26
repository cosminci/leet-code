package io.github.cosminci.leetcode._400

import io.github.cosminci.utils.ListNode

object _328_OddEvenLinkedList:
  def main(args: Array[String]): Unit =
    val result = oddEvenList(
      new ListNode(2, new ListNode(1, new ListNode(3, ListNode(5, new ListNode(6, new ListNode(4, new ListNode(7)))))))
    )
    println(result)

  def oddEvenList(head: ListNode): ListNode =
    if head == null || head.next == null then return head
    val evenHead            = head.next
    var (oddCurr, evenCurr) = (head, evenHead)

    while evenCurr != null && evenCurr.next != null do
      oddCurr.next = evenCurr.next
      oddCurr = oddCurr.next
      evenCurr.next = oddCurr.next
      evenCurr = evenCurr.next

    oddCurr.next = evenHead
    head
