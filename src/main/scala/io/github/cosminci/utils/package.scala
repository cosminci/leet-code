package io.github.cosminci

import scala.io.Source
import scala.util.Using

package object utils:

  def timeSolution[R](opName: String, op: () => R): Unit =
    val time     = System.currentTimeMillis()
    val result   = op()
    val duration = System.currentTimeMillis() - time
    println(s"$opName result: $result. Took ${duration}ms to solve.")

  def loadInputAsListOfStrings(path: String): List[String] =
    Using.resource(Source.fromResource(path))(_.getLines().toList)

  def characterCounts(s: String, upper: Boolean = false): Seq[Int] =
    val (start, end)    = if upper then ('A'.toInt, 'Z'.toInt) else ('a'.toInt, 'z'.toInt)
    val asciiOffset     = start
    val charCountLength = end - asciiOffset + 1
    val charCounts      = Array.fill(charCountLength)(0)
    s.foreach(char => charCounts(char.toInt - asciiOffset) += 1)
    charCounts.toSeq

  def neighbours[T](x: Int, y: Int, grid: Array[Array[T]]): Seq[(Int, Int)] =
    val above = Option.when(x > 0)((x - 1, y))
    val left  = Option.when(y > 0)((x, y - 1))
    val right = Option.when(y < grid(x).length - 1)((x, y + 1))
    val below = Option.when(x < grid.length - 1)((x + 1, y))
    Seq(above, left, right, below).flatten
  
  def isPalindrome(s: String): Boolean =
    var (l, r) = (0, s.length - 1)
    while l < r do
      if s(l) != s(r) then return false
      l += 1
      r -= 1
    true
  
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

  def linkedList(elements: Seq[Int]): ListNode = 
    elements.foldRight[ListNode](null)((value, next) => new ListNode(value, next))

  def seq(head: ListNode): Seq[Int] = 
    if (head == null) Seq.empty
    else head.x +: seq(head.next)
  
  class ListNode(_x: Int = 0, _next: ListNode = null):
    var next: ListNode = _next
    var x: Int         = _x

  class Node(var _value: Int):
    var value: Int           = _value
    var children: List[Node] = List()
