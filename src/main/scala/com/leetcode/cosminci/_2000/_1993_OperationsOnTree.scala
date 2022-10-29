package com.leetcode.cosminci._2000

import scala.collection.mutable

object _1993_OperationsOnTree:
  def main(args: Array[String]): Unit =
    val tree = new LockingTree(Array(-1, 0, 3, 1, 0))
    println(tree.upgrade(4, 5))
    println(tree.upgrade(3, 8))
    println(tree.unlock(0, 7))
    println(tree.lock(2, 7))
    println(tree.upgrade(4, 6))

  class LockingTree(parent: Array[Int]):
    private val lockedBy = Array.fill[Option[Int]](parent.length)(None)
    private val (childToParent, parentToChildren) = parent.zipWithIndex
      .foldLeft(Map.empty[Int, Int], Map.empty[Int, Seq[Int]]) { case ((ctp, ptc), (parent, child)) =>
        (
          ctp.updated(child, parent),
          ptc.updatedWith(parent) {
            case None           => Some(Seq(child))
            case Some(children) => Some(children.appended(child))
          }
        )
      }

    def lock(num: Int, user: Int): Boolean =
      lockedBy(num) match
        case Some(_) =>
          false
        case None =>
          lockedBy(num) = Some(user)
          true

    def unlock(num: Int, user: Int): Boolean =
      lockedBy(num) match
        case Some(userId) if userId == user =>
          lockedBy(num) = None
          true
        case _ =>
          false

    def upgrade(num: Int, user: Int): Boolean =
      lockedBy(num) match
        case Some(_) => false
        case None =>
          if hasLockedAncestor(num) then return false

          val descendantsToUnlock = lockedDescendants(num)
          if descendantsToUnlock.isEmpty then return false

          descendantsToUnlock.foreach { descendantId =>
            lockedBy(descendantId).foreach { user => unlock(descendantId, user) }
          }
          lock(num, user)
          true

    def lockedDescendants(num: Int): Seq[Int] =
      val toVisit = mutable.Queue.from(parentToChildren.getOrElse(num, Seq.empty))
      val result  = mutable.ListBuffer.empty[Int]

      while toVisit.nonEmpty do
        val desc = toVisit.dequeue()
        if lockedBy(desc).isDefined then result.append(desc)
        parentToChildren.getOrElse(desc, Seq.empty).foreach(toVisit.enqueue)

      result.toSeq

    def hasLockedAncestor(num: Int): Boolean =
      var parent = childToParent(num)

      while parent != -1 do
        if lockedBy(parent).isDefined then return true
        parent = childToParent(parent)

      false
