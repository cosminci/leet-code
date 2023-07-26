package com.leetcode.cosminci._2800

object _2785_SortVowelsInString:

  def sortVowels(s: String): String =
    val vowels       = s.zipWithIndex.filter { case (ch, i) => "aeiouAEIOU".contains(ch) }
    val sortedVowels = vowels.map { case (_, i) => i }.zip(vowels.map { case (ch, _) => ch }.sorted).toMap
    s.indices.map(i => if "aeiouAEIOU".contains(s(i)) then sortedVowels(i) else s(i)).mkString
