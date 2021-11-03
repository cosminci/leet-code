package io.github.cosminci.leetcode._400

import io.github.cosminci.utils._

object _328_OddEvenLinkedList:
  def main(args: Array[String]): Unit =
    println(seq(oddEvenList(linkedList(Seq(2, 1, 3, 5, 6, 4, 7)))))

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
