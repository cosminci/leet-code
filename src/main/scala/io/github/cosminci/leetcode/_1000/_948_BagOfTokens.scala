package io.github.cosminci.leetcode._1000

object _948_BagOfTokens:

  def bagOfTokensScore(tokens: Array[Int], power: Int): Int =
    tokens.sortInPlace()

    def dfs(l: Int, r: Int, power: Int, score: Int): Int =
      if l > r then score
      else if power >= tokens(l) then dfs(l + 1, r, power - tokens(l), score + 1)
      else if score > 0 then score.max(dfs(l, r - 1, power + tokens(r), score - 1))
      else score

    dfs(l = 0, r = tokens.indices.last, power, score = 0)
