package com.leetcode.cosminci._200

import scala.util.{Failure, Success, Try}

object _150_EvaluateReversePolishNotation:

  def evalRPN(tokens: Array[String]): Int =
    tokens.foldLeft(Seq.empty[Int]) { (stack, t) =>
      Try(t.toInt) match
        case Success(v) => stack :+ t.toInt
        case _ =>
          val (head, Seq(operand1, operand2)) = stack.splitAt(stack.length - 2)
          t match
            case "+" => head :+ operand1 + operand2
            case "-" => head :+ operand1 - operand2
            case "*" => head :+ operand1 * operand2
            case "/" => head :+ operand1 / operand2
    }
    .head
