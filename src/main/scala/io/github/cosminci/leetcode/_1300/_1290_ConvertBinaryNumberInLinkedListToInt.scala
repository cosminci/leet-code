package io.github.cosminci.leetcode._1300

import io.github.cosminci.utils._

object _1290_ConvertBinaryNumberInLinkedListToInt {
  def getDecimalValue(head: ListNode): Int =
    @annotation.tailrec
    def dfs(node: ListNode, value: Int): Int =
      if (node == null) value
      else dfs(node.next, value * 2 + node.x)
    dfs(head, value = 0)
}
