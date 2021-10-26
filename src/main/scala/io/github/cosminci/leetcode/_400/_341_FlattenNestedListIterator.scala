package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _341_FlattenNestedListIterator {

  trait NestedInteger {
    def isInteger: Boolean
    def getInteger: Int
    def setInteger(i: Int): Unit
    def getList: Array[NestedInteger]
    def add(ni: NestedInteger): Unit
  }

  class NestedIterator(nestedList: List[NestedInteger]) {
    private var stream = lazyIterator(nestedList)

    private def lazyIterator(list: Seq[NestedInteger]): LazyList[Int] =
      list.foldRight(LazyList.empty[Int]) {
        case (ni, acc) =>
          if (ni.isInteger) acc.prepended(ni.getInteger)
          else acc.prependedAll(lazyIterator(ni.getList.toSeq))
      }

    def next(): Int = {
      val result = stream.head
      stream = stream.tail
      result
    }

    def hasNext(): Boolean = stream.headOption.isDefined
  }
}
