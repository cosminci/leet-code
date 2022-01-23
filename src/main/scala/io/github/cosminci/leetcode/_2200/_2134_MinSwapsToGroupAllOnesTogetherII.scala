package io.github.cosminci.leetcode._2200

object _2134_MinSwapsToGroupAllOnesTogetherII:

  def minSwaps(nums: Array[Int]): Int =
    val (total, n)  = (nums.sum, nums.length)
    val initialOnes = nums.slice(0, total).sum

    @annotation.tailrec
    def dfs(l: Int, r: Int, prevOnes: Int, minZeroes: Int): Int =
      if r == n * 2 then minZeroes
      else
        val currOnes = prevOnes + nums(r % n) - nums(l % n)
        dfs(l + 1, r + 1, currOnes, minZeroes.min(total - currOnes))

    dfs(l = 0, r = total, prevOnes = initialOnes, minZeroes = total - initialOnes)
