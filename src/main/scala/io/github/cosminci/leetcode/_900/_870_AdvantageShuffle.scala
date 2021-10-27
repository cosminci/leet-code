package io.github.cosminci.leetcode._900

import scala.collection.mutable

object _870_AdvantageShuffle:
  def main(args: Array[String]): Unit =
    println(advantageCount(Array(2, 0, 4, 1, 2), Array(1, 3, 0, 0, 2)).toSeq)

  private def advantageCount(nums1: Array[Int], nums2: Array[Int]): Array[Int] =
    val nums1Left   = mutable.TreeMap.from(nums1.groupBy(identity).view.mapValues(_.length))
    val permutation = mutable.ListBuffer.empty[Int]

    nums2.foreach { n2 =>
      val n1 = nums1Left.minAfter(n2 + 1) match
        case None              => nums1Left.firstKey
        case Some((higher, _)) => higher
      nums1Left.updateWith(n1) {
        case None | Some(1) => None
        case Some(count)    => Some(count - 1)
      }
      permutation.append(n1)
    }

    permutation.toArray
