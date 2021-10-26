package io.github.cosminci.leetcode._100

import scala.collection.mutable

object _97_InterleavingString:
  private val mem = mutable.Map.empty[(String, String, String), Boolean]

  def main(args: Array[String]): Unit =
    println(isInterleave("aabcc", "dbbca", "aadbbcbcac"))

  private def isInterleave(s1: String, s2: String, s3: String): Boolean =
    if s3.length != s1.length + s2.length then return false
    if s3.isEmpty then return true
    if s1.isEmpty then return s2 == s3
    if s2.isEmpty then return s1 == s3

    if mem.contains((s1, s2, s3)) then return mem((s1, s2, s3))

    val result =
      if s1.head == s3.head && s2.head != s3.head then isInterleave(s1.tail, s2, s3.tail)
      else if s1.head != s3.head && s2.head == s3.head then isInterleave(s1, s2.tail, s3.tail)
      else if s1.head == s3.head && s2.head == s3.head then
        isInterleave(s1.tail, s2, s3.tail) || isInterleave(s1, s2.tail, s3.tail)
      else false

    mem.update((s1, s2, s3), result)

    result
