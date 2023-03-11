package com.leetcode.cosminci._2600

import scala.util.chaining.*

object _2584_SplitArrayToMakeCoprimeProducts:

  def findValidSplit(nums: Array[Int]): Int =
    val right = nums.foldLeft(Map.empty[Int, Int]) { (right, n) =>
      factorize(n).foldLeft(right)((right, f) => right.updated(f, right.getOrElse(f, 0) + 1))
    }
    nums.indices.dropRight(1).foldLeft(0, Map.empty[Int, Int]) { case ((common, left), i) =>
      factorize(nums(i)).foldLeft(common, left) { case ((common, left), f) =>
        val newLeft = left.updated(f, left.getOrElse(f, 0) + 1)
        val toAdd   = if newLeft(f) == 1 then 1 else 0
        val toRem   = if newLeft(f) == right(f) then 1 else 0
        (common + toAdd - toRem, newLeft)
      }.tap { case (common, _) => if common == 0 then return i }
    }
    -1

  private def factorize(n: Int): Seq[Int] =
    val (res, _, rem) = Iterator
      .iterate((Seq.empty[Int], 2, n)) { case (res, i, n) =>
        if n % i > 0 then (res, i + 1 + i % 2, n)
        else (res :+ i, i + 1 + i % 2, Iterator.iterate(n)(_ / i).dropWhile(_ % i == 0).next())
      }.dropWhile { case (_, i, n) => n > 1 && i < 1000 }.next()

    if rem > 1 then res :+ rem else res
