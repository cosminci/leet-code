package io.github.cosminci.leetcode._2200

object _2197_ReplaceNonCoprimeNumsInArray:

  def replaceNonCoprimes(nums: Array[Int]): List[Int] =
    @annotation.tailrec
    def gcd(a: Int, b: Int): Int =
      if b == 0 then a else gcd(b, a % b)

    def lcm(a: Int, b: Int) =
      (a.toLong * b / gcd(a, b)).toInt

    @annotation.tailrec
    def dfs(stack: Array[Int]): Array[Int] =
      if stack.length <= 1 then stack
      else
        val (a, b) = (stack(stack.length - 2), stack.last)
        if gcd(a, b) == 1 then stack
        else dfs(stack.dropRight(2) :+ lcm(a, b))

    // Requires mutable.Stack instead of Array to pass the stricter runtime testcases
    nums.foldLeft(Array.empty[Int])((stack, n) => dfs(stack :+ n)).toList
