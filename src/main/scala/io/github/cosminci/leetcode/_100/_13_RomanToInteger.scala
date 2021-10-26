package io.github.cosminci.leetcode._100

object _13_RomanToInteger:

  def main(args: Array[String]): Unit =
    println(romanToInt("MMMCMXCIII"))

  def romanToInt(s: String): Int =
    var result   = 0
    var idx      = 0
    var currChar = romanValue(s(idx))
    while idx < s.length - 1 do
      val nextChar = romanValue(s(idx + 1))
      if currChar >= nextChar then result += currChar
      else result -= currChar
      currChar = nextChar
      idx += 1
    result + romanValue(s.last)

  private def romanValue(char: Char) = char match
    case 'M' => 1000
    case 'D' => 500
    case 'C' => 100
    case 'L' => 50
    case 'X' => 10
    case 'V' => 5
    case 'I' => 1
