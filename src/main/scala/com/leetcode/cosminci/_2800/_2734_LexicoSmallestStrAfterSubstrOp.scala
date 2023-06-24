package com.leetcode.cosminci._2800

object _2734_LexicoSmallestStrAfterSubstrOp:

  def smallestString(s: String): String =
    s.indices.collectFirst { case i if s(i) != 'a' =>
      val j      = (i + 1 until s.length).find(j => s(j) == 'a').getOrElse(s.length)
      val prefix = s.slice(0, i)
      val middle = (i until j).map(k => (s(k) - 1).toChar).mkString
      val suffix = s.slice(j, s.length)
      s"$prefix$middle$suffix"
    }.getOrElse(s.dropRight(1).appended('z'))
