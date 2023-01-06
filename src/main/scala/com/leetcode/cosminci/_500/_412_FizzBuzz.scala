package com.leetcode.cosminci._500

object _412_FizzBuzz:

  def fizzBuzz(n: Int): List[String] =
    (1 to n).map { i =>
      if i % 15 == 0 then "FizzBuzz"
      else if i % 3 == 0 then "Fizz"
      else if i % 5 == 0 then "Buzz"
      else i.toString
    }.toList
