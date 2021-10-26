package io.github.cosminci.leetcode._100

import io.github.cosminci.utils.ListNode

object _61_RotateList:
  def main(args: Array[String]): Unit =
    val rotated = rotateRight(
      new ListNode(1, new ListNode(2, new ListNode(3, new ListNode(4)))),
      0
    )
    println(rotated)

  private def rotateRight(head: ListNode, k: Int): ListNode =
    if head == null || head.next == null then return head

    var length  = 0
    var tracker = head
    while tracker != null do
      length += 1
      tracker = tracker.next
    val kMod = k % length
    if kMod == 0 then return head

    var relinkIdx = 0
    tracker = head
    while relinkIdx != length - kMod - 1 do
      relinkIdx += 1
      tracker = tracker.next

    val newLast = tracker
    val newHead = tracker.next
    (1 to kMod).foreach(_ => tracker = tracker.next)
    tracker.next = head
    newLast.next = null

    newHead
