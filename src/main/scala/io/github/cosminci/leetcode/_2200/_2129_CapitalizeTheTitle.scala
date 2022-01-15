package io.github.cosminci.leetcode._2200

object _2129_CapitalizeTheTitle {
  def capitalizeTitle(title: String): String =
    title.toLowerCase.split(' ').map(s => if (s.length <= 2) s else s.capitalize).mkString(" ")
}
