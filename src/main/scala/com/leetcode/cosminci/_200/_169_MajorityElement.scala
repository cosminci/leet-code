package com.leetcode.cosminci._200

object _169_MajorityElement:
  def main(args: Array[String]): Unit =
    println(majorityElement(Array(7, 7, 7, 5, 4, 3, 2)))
    println(majorityElementBoyerMoore(Array(7, 7, 7, 5, 4, 3, 7)))

  def majorityElementBoyerMoore(nums: Array[Int]): Int =
    nums.tail
      .foldLeft((nums.head, 1)) { case ((candidate, count), n) =>
        if count == 0 then (n, 1)
        else (candidate, count + (if candidate == n then 1 else -1))
      }._1

  def majorityElement(nums: Array[Int]): Int =
    nums
      .foldLeft(Map.empty[Int, Int])((counts, n) => counts.updated(n, counts.getOrElse(n, 0) + 1))
      .maxBy { case (_, cnt) => cnt }._1
