package io.github.cosminci.leetcode._200

import io.github.cosminci.utils.ListNode

object _147_InsertionSortList:
  def main(args: Array[String]): Unit =
    val sorted = insertionSortList(new ListNode(4, new ListNode(5, new ListNode(1, new ListNode(3)))))
    println(sorted)

  private def insertionSortList(head: ListNode): ListNode =
    val resultDummyHead   = new ListNode(0)
    var (insertion, curr) = (resultDummyHead, head)
    var next: ListNode    = null
    while curr != null do
      next = curr.next

      if insertion.x >= curr.x then insertion = resultDummyHead

      while insertion.next != null && insertion.next.x < curr.x do insertion = insertion.next

      curr.next = insertion.next
      insertion.next = curr
      curr = next

    resultDummyHead.next
