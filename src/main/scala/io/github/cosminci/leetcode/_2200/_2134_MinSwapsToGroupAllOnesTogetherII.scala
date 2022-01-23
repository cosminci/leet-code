package io.github.cosminci.leetcode._2200

object _2134_MinSwapsToGroupAllOnesTogetherII:

  def minSwaps(nums: Array[Int]): Int =
    val (windowSize, n) = (nums.sum, nums.length)
    val initialOnes     = nums.slice(0, windowSize).sum

    @annotation.tailrec
    def dfs(l: Int, prevOnes: Int, minZeroes: Int): Int =
      if l + windowSize == n * 2 then minZeroes
      else
        val currOnes = prevOnes + nums((l + windowSize) % n) - nums(l % n)
        dfs(l + 1, currOnes, minZeroes.min(windowSize - currOnes))

    dfs(l = 0, prevOnes = initialOnes, minZeroes = windowSize - initialOnes)
