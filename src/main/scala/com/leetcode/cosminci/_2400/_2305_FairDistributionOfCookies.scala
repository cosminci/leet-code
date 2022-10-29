package com.leetcode.cosminci._2400

object _2305_FairDistributionOfCookies:

  def distributeCookies(cookies: Array[Int], k: Int): Int =
    def dfs(children: Array[Int], cookiesLeft: Int): Int =
      if cookiesLeft == 0 then children.max
      else
        val unfairness = for
          bag   <- cookies.indices.filter(i => cookiesLeft >> i == 1)
          child <- children.indices
        yield dfs(children.updated(child, children(child) + cookies(bag)), cookiesLeft & ~(1 << bag))
        unfairness.min

    dfs(Array.fill(k)(0), cookies.indices.map(1 << _).sum)
