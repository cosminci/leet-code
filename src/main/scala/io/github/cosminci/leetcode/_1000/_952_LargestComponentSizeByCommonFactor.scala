package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils
import io.github.cosminci.utils.DisjointSetUnion.DSU

import scala.collection.mutable

object _952_LargestComponentSizeByCommonFactor {
  def largestComponentSize(nums: Array[Int]): Int = {
    val dsu  = new DSU

    val primes = mutable.Map.empty[Int, mutable.ListBuffer[Int]]
    for {
      n <- nums
      p <- primeSet(n)
    } primes.getOrElseUpdate(p, mutable.ListBuffer.empty).append(n)

    primes.values.foreach(_.reduce(dsu.union))
    nums.map(dsu.find).groupBy(identity).values.map(_.length).max
  }

  private def primeSet(n: Int): Set[Int] =
    (2 to math.sqrt(n).toInt).collectFirst {
      case i if n % i == 0 =>
        primeSet(n / i) + i
    }.getOrElse(Set(n))
}
