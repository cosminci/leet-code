package io.github.cosminci.leetcode._1000

import io.github.cosminci.utils
import io.github.cosminci.utils.DisjointSetUnion.DSU

object _952_LargestComponentSizeByCommonFactor {
  def largestComponentSize(nums: Array[Int]): Int = {
    val spf = utils.sieve(nums.max + 1)
    val uf  = new DSU
    nums.foreach { n =>
      utils.primeFactors(n, spf).foreach { f =>
        uf.union(n, f)
      }
    }
    uf.ranks.values.max
  }
}
