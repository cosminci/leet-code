package com.leetcode.cosminci._400

object _393_Utf8Validation:

  def validUtf8(data: Array[Int]): Boolean =
    data.foldLeft(0) { (expected, byte) =>
      if expected == 0 then
        if (byte >> 7).toBinaryString == "0" then 0
        else if (byte >> 5).toBinaryString == "110" then 1
        else if (byte >> 4).toBinaryString == "1110" then 2
        else if (byte >> 3).toBinaryString == "11110" then 3
        else return false
      else if (byte >> 6).toBinaryString == "10" then expected - 1
      else return false
    } == 0
