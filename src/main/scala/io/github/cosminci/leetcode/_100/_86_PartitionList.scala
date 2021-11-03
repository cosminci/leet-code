package io.github.cosminci.leetcode._100

import io.github.cosminci.utils._

object _86_PartitionList:
  def main(args: Array[String]): Unit =
    println(seq(partition(linkedList(Seq(1, 4, 2)), 3)))

  def partition(head: ListNode, x: Int): ListNode =
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
