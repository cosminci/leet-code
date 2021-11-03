package io.github.cosminci.leetcode._400

import scala.collection.mutable

object _350_IntersectionOfTwoArraysII:
  def main(args: Array[String]): Unit =
    println(intersect(Array(1, 2, 2, 1), Array(2, 2)).toSeq)

  def intersect(nums1: Array[Int], nums2: Array[Int]): Array[Int] =
    nums2
      .foldLeft(Seq.empty[Int], nums1.groupBy(identity).map { case (k, v) => (k, v.length) }) {
        case ((intersection, remainingNums1Counts), n) =>
          remainingNums1Counts.get(n) match
            case None =>
              (intersection, remainingNums1Counts)
            case Some(1) =>
              (intersection.appended(n), remainingNums1Counts.removed(n))
            case Some(c) =>
              (intersection.appended(n), remainingNums1Counts.updated(n, c - 1))
      }
      ._1
      .toArray
