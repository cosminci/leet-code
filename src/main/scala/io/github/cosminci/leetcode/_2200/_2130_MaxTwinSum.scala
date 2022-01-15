package io.github.cosminci.leetcode._2200

import io.github.cosminci.utils.*

object _2130_MaxTwinSum {
  def main(args: Array[String]): Unit = {
    println(pairSum(linkedList(Seq(5, 4, 3, 3))))
  }

  def pairSum(head: ListNode): Int = {
    @annotation.tailrec
    def length(curr: ListNode, len: Int): Int =
      if (curr == null) len
      else length(curr.next, len + 1)
    val len = length(head, 0)

    @annotation.tailrec
    def findMid(curr: ListNode, idx: Int): ListNode =
      if (idx == len / 2 - 1) curr
      else findMid (curr.next, idx + 1)
    val mid = findMid(head, 0)

    @annotation.tailrec
    def reverse(prev: ListNode, curr: ListNode): ListNode = {
      val next = curr.next
      curr.next = prev
      if (next == null) curr else reverse(curr, next)
    }
    val last = reverse(mid, mid.next)

    (0 until len / 2).foldLeft(head, last, 0) {
      case ((head, last, prevMax), _) =>
        (head.next, last.next, prevMax.max(head.x + last.x))
    }._3
  }
}
