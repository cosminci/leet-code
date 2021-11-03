package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _313_SuperUglyNumber:
  def main(args: Array[String]): Unit =
    println(nthSuperUglyNumber(12, Array(2, 7, 13, 19)))
    println(nthSuperUglyNumber(1, Array(2, 3, 5)))

  def nthSuperUglyNumber(n: Int, primes: Array[Int]): Int =
    case class UglyNumber(value: Int, idx: Int, prime: Int)

    val nextUgly =
      given Ordering[UglyNumber] = (x, y) => y.value.compare(x.value)
      mutable.PriorityQueue.from(primes.map(p => UglyNumber(p, 1, p)))

    val uglyNumbers = Array.tabulate(n)(i => if i == 0 then 1 else 0)
    (1 until n).foreach { i =>
      uglyNumbers(i) = nextUgly.head.value
      while nextUgly.head.value == uglyNumbers(i) do
        val curr = nextUgly.dequeue()
        nextUgly.enqueue(curr.copy(value = curr.prime * uglyNumbers(curr.idx), idx = curr.idx + 1))
    }

    uglyNumbers.last
