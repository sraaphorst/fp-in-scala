package org.vorpal
package mytree

import showable.Showable

enum MyTree[+A]:
  case MyLeaf(value: A)
  case MyBranch(left: MyTree[A], right: MyTree[A])

  def size: Int = this match
    case MyLeaf(_) => 1
    case MyBranch(l, r) => 1 + l.size + r.size

  def depth: Int = this match
    case MyLeaf(_) => 0
    case MyBranch(l, r) => 1 + l.depth.max(r.depth)

  def map[B](f: A => B): MyTree[B] = this match
    case MyLeaf(i) => MyLeaf(f(i))
    case MyBranch(l, r) => MyBranch(l.map(f), r.map(f))

  def fold[B](f: A => B)(g: (B, B) => B): B = this match
    case MyLeaf(a) => f(a)
    case MyBranch(l, r) => g(l.fold(f)(g), r.fold(f)(g))

  // Size using fold
  def sizeF: Int =
    fold(_ => 1)(1 + _ + _)

  // Depth using fold
  def depthF: Int =
    fold(_ => 0)((d1, d2) => 1 + d1.max(d2))

  // Map using fold
  def mapF[B](f: A => B): MyTree[B] =
    fold(a => MyLeaf(f(a)))(MyBranch(_, _))

object MyTree:
  given[A: Showable]: Showable[MyTree[A]] with
    extension (tree: MyTree[A]) def show: String =
      def aux(t: MyTree[A] = tree, level: Int = 0): String =
        val indent = "  " * level
        t match {
          case MyLeaf(value) => s"${indent}MyLeaf(${value.show})"
          case MyBranch(left, right) =>
            s"${indent}MyBranch(\n${aux(left, level + 1)},\n${aux(right, level + 1)}\n${indent})"
        }
      aux()

@main
def main_mytree(): Unit =
  import MyTree.*
  val t = MyBranch(MyBranch(MyLeaf(1), MyLeaf(2)), MyBranch(MyBranch(MyLeaf(3), MyLeaf(4)), MyLeaf(5)))
  println(t.show)
