package com.leetcode.cosminci._2200

object _2129_CapitalizeTheTitle:
  def capitalizeTitle(title: String): String =
    title.toLowerCase.split(' ').map(s => if s.length <= 2 then s else s.capitalize).mkString(" ")
