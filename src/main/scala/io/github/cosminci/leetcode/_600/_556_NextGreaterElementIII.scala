package io.github.cosminci.leetcode._600

object _556_NextGreaterElementIII:
  def main(args: Array[String]): Unit =
    println(nextGreaterElement(12222333))
    println(nextGreaterElement(2147483476))
    println(nextGreaterElement(230241))
    println(nextGreaterElement(1))

  private def nextGreaterElement(n: Int): Int =
    val num = n.toString.toCharArray

    def swap(i: Int, j: Int) =
      val tmp = num(i)
      num(i) = num(j)
      num(j) = tmp

    def reverse(start: Int, end: Int) =
      var (s, e) = (start, end)
      while s < e do
        swap(s, e)
        s += 1
        e -= 1

    (num.length - 2 to 0 by -1)
      .find(i => num(i) < num(i + 1)) match
      case None => -1
      case Some(i) =>
        val smallestGreater = (i + 2 until num.length).foldLeft(i + 1) { case (smallest, j) =>
          if num(j) > num(i) && num(j) <= num(smallest) then j else smallest
        }
        swap(smallestGreater, i)
        reverse(i + 1, num.length - 1)
        if num.mkString.toLong <= Int.MaxValue then num.mkString.toInt else -1
