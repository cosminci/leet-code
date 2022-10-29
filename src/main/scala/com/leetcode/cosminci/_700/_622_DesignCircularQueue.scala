package com.leetcode.cosminci._700

object _622_DesignCircularQueue {
  class MyCircularQueue(capacity: Int) {
    private val queue = Array.ofDim[Int](capacity)
    private var (size, head, last) = (0, 0, 0)

    def enQueue(value: Int): Boolean =
      if (isFull()) false else {
        queue(last) = value
        last = (last + 1) % capacity
        size += 1
        true
      }

    def deQueue(): Boolean =
      if (isEmpty()) false else {
        head = (head + 1) % capacity
        size -= 1
        true
      }

    def Front(): Int = if (isEmpty()) -1 else queue(head)

    def Rear(): Int = if (isEmpty()) -1 else queue(math.floorMod(last - 1, capacity))

    def isEmpty(): Boolean = size == 0

    def isFull(): Boolean = size == capacity
  }
}
