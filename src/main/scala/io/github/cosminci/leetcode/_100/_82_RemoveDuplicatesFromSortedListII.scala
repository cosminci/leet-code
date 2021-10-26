package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _82_RemoveDuplicatesFromSortedListII:
  def main(args: Array[String]): Unit =
    println(deleteDuplicates(new ListNode(1, new ListNode(1))))

  private def deleteDuplicates(head: ListNode): ListNode =
    val newDummyHead = new ListNode(Int.MaxValue, null)
    var newCurr      = newDummyHead
    var (prev, curr) = (newDummyHead, head)

    while curr != null do
      if curr.x != prev.x && Option(curr.next).forall(_.x != curr.x) then
        newCurr.next = curr
        newCurr = curr
      prev = curr
      curr = curr.next

    newCurr.next = null
    newDummyHead.next
