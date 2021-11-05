package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _900_RLE_Iterator {

  def main(args: Array[String]): Unit = {
    val it = new RLEIterator(Array(3, 8, 0, 9, 2, 5))
    println(it.next(2))
    println(it.next(1))
    println(it.next(1))
    println(it.next(2))
  }

  class RLEIterator(encoding: Array[Int]) {
    private val stack = mutable.Stack.from(encoding.grouped(2).collect {
      case Array(count, num) if count > 0 => (num, count)
    })

    @annotation.tailrec
    final def next(n: Int): Int = {
      if (stack.isEmpty) -1
      else {
        val (num, count) = stack.pop()
        if (count < n) next(n - count)
        else {
          if (count > n) stack.push((num, count - n))
          num
        }
      }
    }
  }
}
