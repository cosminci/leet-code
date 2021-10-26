package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _155_MinStack:

  class MinStack {
    private val stack = mutable.Stack.empty[(Int, Int)]

    def push(value: Int): Unit =
      stack.headOption match {
        case None         => stack.push((value, value))
        case Some((_, m)) => stack.push((value, value.min(m)))
      }

    def pop(): Unit   = stack.pop()
    def top(): Int    = stack.head._1
    def getMin(): Int = stack.head._2
  }
