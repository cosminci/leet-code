package io.github.cosminci.leetcode._200

object _169_MajorityElement:
  def main(args: Array[String]): Unit =
    println(majorityElement(Array(7, 7, 7, 5, 4, 3, 2)))
    println(majorityElementBoyerMoore(Array(7, 7, 7, 5, 4, 3, 7)))

  private def majorityElementBoyerMoore(nums: Array[Int]): Int =
    nums.tail
      .foldLeft((nums.head, 1)) { case ((candidate, count), n) =>
        if count == 0 then (n, 1)
        else (candidate, count + (if candidate == n then 1 else -1))
      }
      ._1

  private def majorityElement(nums: Array[Int]): Int =
    nums
      .foldLeft(Map.empty[Int, Int]) { (counts, n) =>
        counts.updatedWith(n) {
          case None    => Some(1)
          case Some(c) => Some(c + 1)
        }
      }
      .maxBy(_._2)
      ._1
