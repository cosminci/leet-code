package com.leetcode.cosminci._2400

object _2325_DecodeTheMessage:

  def decodeMessage(key: String, message: String): String =
    val table = ('a' to 'z')
      .sortBy(c => key.indexOf(c))
      .zip('a' to 'z')
      .toMap + (' ' -> ' ')

    message.map(table)
