package com.leetcode.cosminci._300

object _227_BasicCalculatorII:
  def main(args: Array[String]): Unit =
    println(calculate("7/ 3+5 / 2 "))
    println(calculate("3+2*2"))
    println(calculate(" 3 / 2 "))

  def calculate(s: String): Int =
    @annotation.tailrec
    def dfs(i: Int, prevVal: Int, prevOp: Char, result: Int): Int =
      if i >= s.length then result + prevVal
      else if s(i) == ' ' then dfs(i + 1, prevVal, prevOp, result)
      else if !s(i).isDigit then dfs(i + 1, prevVal, s(i), result)
      else
        val endIdx  = s.indexWhere(c => !c.isDigit, i)
        val newIdx  = Option.when(endIdx > 0)(endIdx).getOrElse(s.length)
        val currVal = s.slice(i, newIdx).toInt
        val (newVal, newResult) = prevOp match
          case '+' => (currVal, result + prevVal)
          case '-' => (-currVal, result + prevVal)
          case '*' => (prevVal * currVal, result)
          case _   => (prevVal / currVal, result)
        dfs(newIdx, newVal, prevOp, newResult)

    dfs(i = 0, prevVal = 0, prevOp = '+', result = 0)
