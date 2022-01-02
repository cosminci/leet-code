package io.github.cosminci.leetcode._2200

object _2122_RecoverTheOriginalArray:
  def recoverArray(nums: Array[Int]): Array[Int] =
    nums.sortInPlace()
    val counter = nums.groupMapReduce(identity)(_ => 1)(_ + _).withDefaultValue(0)

    def check(diff: Int): Option[Array[Int]] =
      Some(nums.foldLeft(counter, Array.empty[Int]) { case ((cnt, res), n) =>
        if cnt(n) == 0 then (cnt, res)
        else if cnt(n + diff) == 0 then return None
        else (cnt.updated(n, cnt(n) - 1).updated(n + diff, cnt(n + diff) - 1), res :+ (n + diff / 2))
      }._2)

    (1 until nums.length).view
      .map(i => nums(i) - nums.head)
      .filter(diff => diff != 0 && diff % 2 == 0)
      .flatMap(check)
      .head
