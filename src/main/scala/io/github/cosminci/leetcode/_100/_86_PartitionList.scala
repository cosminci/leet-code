package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _86_PartitionList:
  def main(args: Array[String]): Unit =
    val result = partition(new ListNode(1, new ListNode(4, new ListNode(2))), 3)
    println(result)

  private def partition(head: ListNode, x: Int): ListNode =
    val smallerHead = new ListNode(-1, null)
    val largerHead  = new ListNode(-1, null)

    var (curr, smaller, larger) = (head, smallerHead, largerHead)
    while curr != null do
      if curr.x < x then
        smaller.next = curr
        smaller = smaller.next
      else
        larger.next = curr
        larger = larger.next
      curr = curr.next

    larger.next = null
    smaller.next = largerHead.next
    smallerHead.next
