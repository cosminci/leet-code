package io.github.cosminci.leetcode._100

import io.github.cosminci.utils._

object _82_RemoveDuplicatesFromSortedListII:
  def main(args: Array[String]): Unit =
    println(seq(deleteDuplicates(linkedList(Seq(1, 1)))))

  def deleteDuplicates(head: ListNode): ListNode =
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
