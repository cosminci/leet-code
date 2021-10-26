package io.github.cosminci.leetcode._200

import io.github.cosminci.utils

import scala.collection.mutable

object _131_PalindromePartitioning:
  private val mem = mutable.Map.empty[String, List[List[String]]]

  def main(args: Array[String]): Unit =
    println(partition("bb"))

  private def partition(s: String): List[List[String]] =
    if s.length == 1 then return List(List(s.head.toString))
    if mem.contains(s) then return mem(s)

    val result = s.indices.flatMap { idx =>
      val prefix = s.take(idx + 1)
      if utils.isPalindrome(prefix) then
        if prefix.length == s.length then List(List(prefix))
        else partition(s.substring(idx + 1)).map(_.prepended(prefix))
      else List.empty
    }.toList

    mem.update(s, result)
    result
