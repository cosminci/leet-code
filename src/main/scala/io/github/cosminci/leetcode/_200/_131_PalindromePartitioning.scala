package io.github.cosminci.leetcode._200

import io.github.cosminci.utils

import scala.collection.mutable

object _131_PalindromePartitioning:

  def main(args: Array[String]): Unit =
    println(partition("aab"))

  def partition(s: String): List[List[String]] =
    val mem = mutable.Map.empty[String, List[List[String]]]
    def dfs(s: String): List[List[String]] = mem.getOrElseUpdate(s, {
      if s.length == 1 then List(List(s))
      else s.view.indices
        .filter(idx => utils.isPalindrome(s.take(idx + 1)))
        .map(idx => s.splitAt(idx + 1))
        .flatMap { case (prefix, rest) =>
          Option
            .when(prefix.length == s.length)(List(List(prefix)))
            .getOrElse(partition(rest).map(_.prepended(prefix)))
        }.toList
      }
    )
    dfs(s)
