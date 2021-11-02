package io.github.cosminci.leetcode._200

import io.github.cosminci.utils._

object _147_InsertionSortList:
  def main(args: Array[String]): Unit =
    println(seq(insertionSortList(linkedList(Seq(4, 5, 1, 3)))))

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
