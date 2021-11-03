package io.github.cosminci.leetcode._100

import io.github.cosminci.utils._

object _92_ReverseLinkedListII:
  def main(args: Array[String]): Unit =
    println(seq(reverseBetween(linkedList(Seq(5, 1)), 1, 2)))

  def reverseBetween(head: ListNode, left: Int, right: Int): ListNode =
    if right - left < 1 then return head

    val dummy = new ListNode(0, head)

    var (prev, curr, count) = (dummy, head, 1)
    while count < left do
      prev = curr
      curr = curr.next
      count += 1

    val startHead = prev
    prev = curr
    curr = curr.next

    while count < right do
      val next = curr.next
      curr.next = prev
      prev = curr
      curr = next
      count += 1

    startHead.next.next = curr
    startHead.next = prev

    dummy.next
