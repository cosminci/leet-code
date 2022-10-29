package com.leetcode.cosminci._2500

object _2452_WordsWithTwoEditsOfDictionary:

  def twoEditWords(queries: Array[String], dictionary: Array[String]): List[String] =
    def hammingLessThanTwo(w1: String, w2: String): Boolean =
      @annotation.tailrec
      def dfs(i: Int, diff: Int): Boolean =
        if diff > 2 then false
        else if i == w1.length then true
        else dfs(i + 1, diff + Option.when(w1(i) == w2(i))(0).getOrElse(1))
      dfs(i = 0, diff = 0)

    queries.filter(w1 => dictionary.exists(w2 => hammingLessThanTwo(w1, w2))).toList
