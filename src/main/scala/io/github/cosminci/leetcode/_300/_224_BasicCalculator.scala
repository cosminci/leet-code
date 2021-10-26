package io.github.cosminci.leetcode._300

import scala.collection.mutable

object _224_BasicCalculator:
  def main(args: Array[String]): Unit =
    println(calculate("( -1)"))
    println(calculate("(1+(4+5+2)-3)+(6+8)"))

  private def calculate(s: String): Int =
    val stack = mutable.Stack.empty[Int]

    var (result, operand, sign) = (0, 0, 1)
    s.foreach { c =>
      if c.isDigit then operand = operand * 10 + (c - '0')
      else if c == '+' then
        result += sign * operand
        operand = 0
        sign = 1
      else if c == '-' then
        result += sign * operand
        operand = 0
        sign = -1
      else if c == '(' then
        stack.push(result)
        stack.push(sign)
        sign = 1
        result = 0
      else if c == ')' then
        result += sign * operand
        operand = 0
        result *= stack.pop() // sign
        result += stack.pop() // prev result
    }
    
    result + sign * operand
