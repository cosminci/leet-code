package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _19_RemoveNthNodeFromEndOfList:
  def main(args: Array[String]): Unit =
    val result = removeNthFromEnd(new ListNode(1), 1)
    println(result)

  private def removeNthFromEnd(head: ListNode, n: Int): ListNode =
    var dummy           = new ListNode(666, head)
    var (behind, ahead) = (dummy, dummy)

    (0 to n).foreach { _ => ahead = ahead.next }
    while ahead != null do
      ahead = ahead.next
      behind = behind.next

    behind.next = behind.next.next
    dummy.next
