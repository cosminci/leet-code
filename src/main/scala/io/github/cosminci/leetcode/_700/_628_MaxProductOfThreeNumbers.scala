package io.github.cosminci.leetcode._700

import java.util.Comparator
import scala.jdk.CollectionConverters.*
import scala.util.Try

object _628_MaxProductOfThreeNumbers:
  def main(args: Array[String]): Unit =
    println(maximumProduct(Array(1, 2, 3, 4)))
    println(maximumProduct(Array(-2, -1, 4, 5, -1)))
    println(maximumProduct(Array(-2, -1, -3, -7)))
    println(maximumProduct(Array(1000, 1000, 1000)))

  private def maximumProduct(nums: Array[Int]): Int =
    val comparator: Comparator[Int] = (i, j) =>
      val valueComparison = nums(i).compare(nums(j))
      if valueComparison == 0 then i.compare(j) else valueComparison
    var minTwo   = new java.util.TreeSet[Int](comparator)
    var maxThree = new java.util.TreeSet[Int](comparator)

    nums.indices.foreach { i =>
      if maxThree.size() < 3 then maxThree.add(i)
      else if Try(maxThree.first()).toOption.forall(h => nums(h) < nums(i)) then
        if maxThree.size == 3 then maxThree.pollFirst()
        maxThree.add(i)
      if minTwo.size < 2 then minTwo.add(i)
      else if Try(minTwo.last()).toOption.forall(h => nums(h) > nums(i)) then
        if minTwo.size == 2 then minTwo.pollLast()
        minTwo.add(i)
    }

    math.max(
      minTwo.asScala.toList.map(nums).product * nums(maxThree.last),
      maxThree.asScala.toList.map(nums).product
    )
