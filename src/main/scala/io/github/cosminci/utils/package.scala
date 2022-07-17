package io.github.cosminci

import scala.collection.mutable
import scala.io.Source
import scala.math.Integral.Implicits._
import scala.util.Using

package object utils:
  
  @annotation.tailrec
  def gcd[T: Integral](a: T, b: T): T = if b == 0 then a else gcd(b, a % b)

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
  
  def isPalindrome(s: String): Boolean = {
    @annotation.tailrec
    def dfs(l: Int, r: Int): Boolean =
      l >= r || (s(l) == s(r) && dfs(l + 1, r - 1))

    dfs(l = 0, r = s.length - 1)
  }
  
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
    if (head == null) Seq.empty
    else head.x +: linkedListToSeq(head.next)
  
  class ListNode(_x: Int = 0, _next: ListNode = null):
    var next: ListNode = _next
    var x: Int         = _x

  class Node(var _value: Int):
    var value: Int           = _value
    var children: List[Node] = List()
