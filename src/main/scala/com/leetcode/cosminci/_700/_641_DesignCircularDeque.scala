package com.leetcode.cosminci._700

import scala.collection.mutable

object _641_DesignCircularDeque {

  class MyCircularDeque(capacity: Int) {
    private val buffer = Array.ofDim[Int](capacity)
    private var (front, rear) = (capacity - 1, 0)
    private var size = 0

    def insertFront(value: Int): Boolean =
      if (isFull()) false
      else {
        buffer(front) = value
        front = math.floorMod(front - 1, capacity)
        size += 1
        true
      }

    def insertLast(value: Int): Boolean =
      if (isFull()) false
      else {
        buffer(rear) = value
        rear = (rear + 1) % capacity
        size += 1
        true
      }

    def deleteFront(): Boolean =
      if (isEmpty()) false else {
        front = (front + 1) % capacity
        size -= 1
        true
      }

    def deleteLast(): Boolean =
      if (isEmpty()) false else {
        rear = math.floorMod(rear - 1, capacity)
        size -= 1
        true
      }

    def getFront(): Int =
      if (isEmpty()) -1 else buffer((front + 1) % capacity)

    def getRear(): Int =
      if (isEmpty()) -1 else buffer(math.floorMod(rear - 1, capacity))

    def isEmpty(): Boolean = size == 0

    def isFull(): Boolean = size == capacity
  }
  
}
