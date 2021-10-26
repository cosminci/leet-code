package io.github.cosminci.leetcode._300

object _229_MajorityElementII:
  def main(args: Array[String]): Unit =
    println(majorityElementBoyerMoore(Array(2, 2, 1, 3)))
    println(majorityElement(Array(2, 2, 1, 3)))

  private def majorityElementBoyerMoore(nums: Array[Int]): List[Int] =
    val (candidate1, count1, candidate2, count2) =
      nums.foldLeft((0, 0, 1, 0)) { case ((cand1, cnt1, cand2, cnt2), n) =>
        if cand1 == n then (cand1, cnt1 + 1, cand2, cnt2)
        else if cand2 == n then (cand1, cnt1, cand2, cnt2 + 1)
        else if cnt1 == 0 then (n, 1, cand2, cnt2)
        else if cnt2 == 0 then (cand1, cnt1, n, 1)
        else (cand1, cnt1 - 1, cand2, cnt2 - 1)
      }
    List(candidate1, candidate2).filter(candidate => nums.count(_ == candidate) > nums.length / 3.0)

  private def majorityElement(nums: Array[Int]): List[Int] =
    nums
      .groupBy(identity)
      .view
      .mapValues(_.size)
      .collect {
        case (n, count) if count > nums.size / 3.0 => n
      }
      .toList
