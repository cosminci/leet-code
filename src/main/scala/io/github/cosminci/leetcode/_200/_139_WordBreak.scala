package io.github.cosminci.leetcode._200

import scala.collection.mutable

object _139_WordBreak:
  def main(args: Array[String]): Unit =
    println(wordBreakTopDown("leetcode", List("leet", "code")))
    println(wordBreakBottomUp("leetcode", List("leet", "code")))
    println(wordBreakTopDown("catsandog", List("cats", "dog", "sand", "and", "cat")))
    println(wordBreakBottomUp("catsandog", List("cats", "dog", "sand", "and", "cat")))
    println(wordBreakTopDown("applepenapple", List("apple", "pen")))
    println(wordBreakBottomUp("applepenapple", List("apple", "pen")))

  private def wordBreakBottomUp(s: String, wordDict: List[String]): Boolean =
    val dictionary = wordDict.toSet

    val dp = Array.ofDim[Boolean](s.length + 1)
    dp(s.length) = true

    (s.length - 1 to 0 by -1).foreach { i =>
      dp(i) = dictionary.exists { w =>
        s.substring(i).startsWith(w) && dp(i + w.length)
      }
    }

    dp.head

  private def wordBreakTopDown(input: String, wordDict: List[String]): Boolean =
    val dictionary = wordDict.toSet

    val mem = mutable.Map.empty[String, Boolean]
    def dfs(s: String): Boolean =
      if s.isEmpty then return false
      if dictionary.contains(s) then return true
      if mem.contains(s) then return mem(s)

      val result = dictionary.exists { w =>
        s.startsWith(w) && dfs(s.substring(w.length))
      }
      mem.update(s, result)
      result

    dfs(input)
