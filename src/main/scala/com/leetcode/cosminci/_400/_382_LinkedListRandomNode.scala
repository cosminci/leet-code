package com.leetcode.cosminci._400

import com.leetcode.cosminci.utils.ListNode

import scala.util.Random

object _382_LinkedListRandomNode:

  class Solution(head: ListNode):
    def getRandom(): Int =
      var (curr, reservoir, idx) = (head, head.x, 1)

      while curr.next != null do
        curr = curr.next
        if Random.nextInt(idx + 1) == idx then reservoir = curr.x
        idx += 1

      reservoir
