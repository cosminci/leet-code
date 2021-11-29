package io.github.cosminci.leetcode._2000

import io.github.cosminci.utils
import io.github.cosminci.utils.UnionFind

import scala.collection.mutable

object _1998_GcdSortOfAnArray:

  def main(args: Array[String]): Unit =
    println(gcdSort(utils.loadInputAsListOfStrings("1998.txt").head.split(",").map(_.toInt)))

  def gcdSort(nums: Array[Int]): Boolean =
    val spf = sieve(nums.max + 1)
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

  private def sieve(n: Int): Seq[Int] =
    val spf = Array.tabulate(n)(i => i) // init smallest prime factor to the number itself
    (2 to math.sqrt(n).toInt).foreach { i =>
      if spf(i) == i then // only if prime
        (i * i until n by i).foreach { j =>
          if spf(j) > i then spf(j) = i // update smallest prime factor for all multiples
        }
    }
    spf.toSeq
  