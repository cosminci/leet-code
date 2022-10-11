package io.github.cosminci.leetcode._400

object _334_IncreasingTripletSubseq:

  def increasingTripletScan(nums: Array[Int]): Boolean =
    val maxSuffix = nums.scanRight(0)(math.max)
    val minPrefix = nums.scanLeft(Int.MaxValue)(math.min)
    (1 until nums.length).exists { i =>
      minPrefix(i) < nums(i) && nums(i) < maxSuffix(i + 1)
    }

  def increasingTripletFold(nums: Array[Int]): Boolean =
    nums.foldLeft(Int.MaxValue, Int.MaxValue) { case ((min1, min2), n) =>
      if n <= min1 then (n, min2)
      else if n <= min2 then (min1, n)
      else return true
    }
    false

  def increasingTripletRecursive(nums: Array[Int]): Boolean =
    @annotation.tailrec
    def dfs(idx: Int, min1: Int, min2: Int): Boolean =
      if idx == nums.length then false
      else if nums(idx) <= min1 then dfs(idx + 1, nums(idx), min2)
      else if nums(idx) <= min2 then dfs(idx + 1, min1, nums(idx))
      else true
    dfs(idx = 0, min1 = Int.MaxValue, min2 = Int.MaxValue)
