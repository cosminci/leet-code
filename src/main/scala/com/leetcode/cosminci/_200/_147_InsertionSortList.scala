package com.leetcode.cosminci._200

import com.leetcode.cosminci.utils.*

object _147_InsertionSortList:
  def main(args: Array[String]): Unit =
    println(linkedListToSeq(insertionSortList(seqToLinkedList(Seq(4, 5, 1, 3)))))

  def insertionSortList(head: ListNode): ListNode =
    val dummyHead               = new ListNode(0)
    var (insertion, curr, next) = (dummyHead, head, head)

    while curr != null do
      next = curr.next

      if insertion.x >= curr.x then insertion = dummyHead

      while insertion.next != null && insertion.next.x < curr.x do insertion = insertion.next

      curr.next = insertion.next
      insertion.next = curr
      curr = next

    dummyHead.next
