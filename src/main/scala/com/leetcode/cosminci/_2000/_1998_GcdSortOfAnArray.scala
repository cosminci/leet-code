package com.leetcode.cosminci._2000

import com.leetcode.cosminci.utils.{sieveOfEratosthenes, UnionFind}

import scala.collection.mutable

object _1998_GcdSortOfAnArray:

  def gcdSort(nums: Array[Int]): Boolean =
    val spf = sieveOfEratosthenes(nums.max + 1)
    val uf  = new UnionFind[Int]
    nums.foreach { n =>
      primeFactors(n, spf).foreach { f =>
        uf.union(n, f)
      }
    }
    nums.zip(nums.sorted).forall { case (x, y) =>
      uf.find(x) == uf.find(y)
    }

  private def primeFactors(n: Int, spf: Seq[Int]): Seq[Int] =
    val factors = mutable.ListBuffer.empty[Int]
    var num     = n
    while num > 1 do
      factors.append(spf(num))
      num /= spf(num)
    factors.toSeq
