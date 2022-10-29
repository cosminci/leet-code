package com.leetcode.cosminci._600

import scala.collection.mutable
import scala.util.Random

object _519_RandomFlipMatrix {

  class Solution(m: Int, n: Int) {
    private val map = mutable.Map.empty[Int, Int]
    private var total = m * n

    def flip(): Array[Int] = {
      val r = Random.nextInt(total)
      total -= 1
      val x = map.getOrElse(r, r)
      map.update(r, map.getOrElse(total, total))
      Array(x / n, x % n)
    }

    def reset(): Unit = {
      total = m * n
      map.clear()
    }
  }
}
