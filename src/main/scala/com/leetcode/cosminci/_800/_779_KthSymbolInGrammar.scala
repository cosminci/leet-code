package com.leetcode.cosminci._800

object _779_KthSymbolInGrammar:
  def main(args: Array[String]): Unit =
    println(kthGrammar(30, 434991989))

  def kthGrammar(n: Int, k: Int): Int =
    if n == 1 then 0
    else if k % 2 == 0 then 1 - kthGrammar(n - 1, k / 2)
    else kthGrammar(n - 1, (k + 1) / 2)
