package com.leetcode.cosminci._300

object _282_ExpressionAddOperators:
  def main(args: Array[String]): Unit =
    println(addOperators("123", 6))
    println(addOperators("232", 8))
    println(addOperators("105", 5))
    println(addOperators("00", 0))
    println(addOperators("3456237490", 9191))

  def addOperators(num: String, target: Int): List[String] =
    def dfs(idx: Int, path: String, pathValue: Long, maybePrevOperand: Option[Long]): Seq[String] =
      if idx == num.length && pathValue == target then return Seq(path)

      (idx + 1 to num.length).flatMap { i =>
        val remainingDigits = num.substring(idx, i)
        if i > idx + 1 && num(idx) == '0' then Seq.empty
        else
          val operand = remainingDigits.toLong
          maybePrevOperand match
            case None =>
              dfs(i, remainingDigits, operand, Some(operand))
            case Some(prev) =>
              dfs(i, s"$path+$remainingDigits", pathValue + operand, Some(operand)) ++
                dfs(i, s"$path-$remainingDigits", pathValue - operand, Some(-operand)) ++
                dfs(i, s"$path*$remainingDigits", pathValue - prev + prev * operand, Some(prev * operand))
      }

    dfs(idx = 0, path = "", pathValue = 0, maybePrevOperand = None).toList
