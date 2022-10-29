package com.leetcode.cosminci._300

object _284_PeekingIterator:
  class PeekingIterator(iterator: Iterator[Int]):
    private var peeked: Option[Int] = None

    def peek(): Int = peeked match
      case None =>
        val value = iterator.next()
        peeked = Some(value)
        value
      case Some(value) => value

    def next(): Int = peeked match
      case None =>
        iterator.next()
      case Some(value) =>
        peeked = None
        value

    def hasNext(): Boolean = iterator.hasNext || peeked.isDefined
