package io.github.cosminci.leetcode._300

import io.github.cosminci.utils._

object _234_PalindromeLinkedList:
  def main(args: Array[String]): Unit =
    println(isPalindrome(seqToLinkedList(Seq(1, 2, 2, 1))))

  def isPalindrome(head: ListNode): Boolean =
    var prev: ListNode = null
    var (slow, fast)   = (head, head)

    // reverse links up to mid
    while fast != null && fast.next != null do
      fast = fast.next.next
      val next = slow.next
      slow.next = prev
      prev = slow
      slow = next

    // skip middle element if odd number of elements
    if fast != null then slow = slow.next

    while prev != null && slow.x == prev.x do
      slow = slow.next
      prev = prev.next
    prev == null
