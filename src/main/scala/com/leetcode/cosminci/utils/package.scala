package com.leetcode.cosminci

import scala.collection.mutable
import scala.io.Source
import scala.math.Integral.Implicits.*
import scala.util.Using

package object utils:

  def powMod(base: Int, pow: Int, mod: Int): Int =
    @annotation.tailrec
    def dfs(base: Long, pow: Int, result: Long): Int =
      if pow == 0 then result.toInt
      else if pow % 2 == 0 then dfs((base * base) % mod, pow / 2, result)
      else dfs((base * base) % mod, pow / 2, (result * base) % mod)

    dfs(base, pow, result = 1L)

  @annotation.tailrec
  def gcd[T: Integral](a: T, b: T): T = if b == 0 then a else gcd(b, a % b)

  def lcm[T: Integral](a: T, b: T): T = a / gcd(a, b) * b

  def bisectLeft(nums: collection.Seq[Int], n: Int): Int =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Int =
      if l >= r then l
      else
        val mid = l + (r - l) / 2
        if nums(mid) < n then dfs(mid + 1, r)
        else dfs(l, mid)
    dfs(l = 0, r = nums.length)

  def characterCounts(s: String, upper: Boolean = false): Seq[Int] =
    val (start, end)    = if upper then ('A'.toInt, 'Z'.toInt) else ('a'.toInt, 'z'.toInt)
    val asciiOffset     = start
    val charCountLength = end - asciiOffset + 1
    val charCounts      = Array.fill(charCountLength)(0)
    s.foreach(char => charCounts(char.toInt - asciiOffset) += 1)
    charCounts.toSeq

  def neighbours[T](x: Int, y: Int, grid: Array[Array[T]]): Seq[(Int, Int)] =
    Seq((-1, 0), (0, -1), (0, 1), (1, 0)).collect {
      case (dx, dy) if x + dx >= 0 && x + dx < grid.length && y + dy >= 0 && y + dy < grid.head.length =>
        (x + dx, y + dy)
    }
    
  def decrementCounter[A](cnt: Map[A, Int], key: A): Map[A, Int] =
    cnt.updatedWith(key) {
      case Some(c) if c > 1 => Some(c - 1)
      case _ => None
    }

  def isPalindrome(s: String): Boolean =
    @annotation.tailrec
    def dfs(l: Int, r: Int): Boolean =
      l >= r || (s(l) == s(r) && dfs(l + 1, r - 1))

    dfs(l = 0, r = s.length - 1)

  def sieveOfEratosthenes(n: Int): Seq[Int] =
    val spf = Array.tabulate(n)(i => i)
    (2 to math.sqrt(n).toInt).foreach { i =>
      if spf(i) == i then
        (i * i until n by i).foreach { j =>
          if spf(j) > i then spf(j) = i
        }
    }
    spf.toSeq

  class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null):
    var value: Int      = _value
    var left: TreeNode  = _left
    var right: TreeNode = _right

    private def canEqual(other: Any): Boolean = other.isInstanceOf[TreeNode]

    override def equals(other: Any): Boolean = other match
      case that: TreeNode =>
        (that canEqual this) &&
          value == that.value &&
          left == that.left &&
          right == that.right
      case _ => false

  def seqToLinkedList(elements: Seq[Int]): ListNode =
    elements.foldRight[ListNode](null)((value, next) => new ListNode(value, next))

  def linkedListToSeq(head: ListNode): Seq[Int] =
    if head == null then Seq.empty
    else head.x +: linkedListToSeq(head.next)

  class ListNode(_x: Int = 0, _next: ListNode = null):
    var next: ListNode = _next
    var x: Int         = _x

  class Node(var _value: Int):
    var value: Int           = _value
    var children: List[Node] = List()
