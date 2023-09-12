package org.vorpal
package mylist

import scala.annotation.tailrec
import showable.Showable
import myoption.MyOption.*

enum MyList[+A]:
  case MyNil
  case MyCons(head: A, tail: MyList[A])

  def hd: Option[A] = this match
    case MyNil => None
    case MyCons(x, _) => Some(x)

  def tl: MyList[A] = this match
    case MyNil => this
    case MyCons (_, xs) => xs

  /**
   * Variance issues:
   *
   * val animalList: MyList[Animal] = new MyList[Animal] { /* implementation here */ }
   * val dogList: MyList[Dog] = new MyList[Dog] { /* implementation here */ }
   *
   * animalList.add(dogList)  // This should be problematic
   * Since Dog is a subtype of Animal, due to covariance, MyList[Dog] is a subtype of MyList[Animal].
   * If the add method were allowed, you could end up adding a MyList[Dog] to a MyList[Animal], which breaks type safety.
   *
   * Thus, we must make B a subtype of A explicitly.
   * This allows widening the type.
   */
  def setHead[B >: A](b: B): MyList[B] = this match
    case MyNil => MyNil
    case MyCons(_, xs) => MyCons(b, xs)

  def isEmpty: Boolean = this match
    case MyNil => true
    case MyCons(_, _) => false

  def isNonEmpty: Boolean = !isEmpty

  def forEach(f: A => Unit): Unit = this match
    case MyNil => ()
    case MyCons(x, xs) => f(x) ; xs.forEach(f)

  def map[B](f: A => B): MyList[B] = this match
    case MyNil => MyNil
    case MyCons(x, xs) => MyCons(f(x), xs.map(f))

  def flatMap[B](f: A => MyList[B]): MyList[B] =
    this.map(f).concatenate()

  def filter(p: A => Boolean): MyList[A] = this match
    case MyNil => MyNil
    case MyCons(x, xs) => if p(x) then MyCons(x, xs.filter(p)) else xs.filter(p)

  def drop(n: Int): MyList[A] = (n, this) match
    case (0, lst) => this
    case (_, MyNil) => MyNil
    case (n, MyCons(_, xs)) => this.drop(n - 1)

  def dropWhile(p: A => Boolean): MyList[A] = this match
    case MyNil => MyNil
    case MyCons(x, xs) if p(x) => xs.dropWhile(p)
    case _ => this

  def append[B >: A](other: MyList[B]): MyList[B] = this match
    case MyNil => other
    case MyCons(x, xs) => MyCons(x, xs.append(other))

  // Get the list minus the last element.
  def init: MyList[A] = this match
    case MyNil => MyNil
    case MyCons(_, MyNil) => MyNil
    case MyCons(x, xs) => MyCons(x, xs.init)

  // This cannot be written to be stack-safe.
  // If we have a list [a0, a1, a2], this computes:
  // f(a0, f(a1, f(a2, z)))
  def foldRight[B](z: B)(f: (A, B) => B): B = this match
    case MyNil => z
    case MyCons(x, xs) => f(x, xs.foldRight(z)(f))

  // Write foldRight in terms of foldLeft?
  //   def foldRight[A, B](lst: MyList[A], z: B)(f: (A, B) => B): B = lst match {
  // We build a function from B to B, starting with the identity, and at each step, we enhance this
  // with f(b, a) for the element a of the list.
  // Remember that f(b, a) gives b.
  // Thus, if our list is [a0 a1 a2], we start with the foldRight:
  // a = a2, acc = id[B] -> new acc = f(b, a2) . id[B]
  // a = a1, acc = f(b, a2) . id[B] -> new acc = f(b, a1) . f(b, a2) . id[B]
  // a = a0, acc = f(b, a1) . f(b, a2) . id[B] -> new acc = f(b, a0) . f(b, a1) . f(b, a2) . id[B]
  //
  // acc is still a function B => B with partial applications for values of the list.
  //
  // Then we evaluate at z: B, which gives us:
  //
  // f(b, a0) . f(b, a1) . f(b, a2) . id[B] (z)
  // = f(b, a0) . f(b, a1) . f(b, a2) (z)
  // = f(b, a0) . f(b, a1) . f(z, a2)
  // = f(b, a0) . f(f(z, a2), a1)
  // = f(f(f(z, a2), a1), a0)
  // which is the answer we want.
  def foldLeftViaRight[B](z: B)(f: (B, A) => B): B =
    val func = this.foldRight[B => B](identity[B]){ (a, acc) => acc.compose(b => f(b, a)) }
    func(z)

  // If we have a list [a0, a1, a2], this computes, starting from left a0 to right a2 as:
  // f(f(f(z, a0), a1), a2)
  def foldLeft[B](z: B)(f: (B, A) => B): B =
    @tailrec
    def aux(acc: B = z, lst: MyList[A] = this): B = lst match
      case MyNil => acc
      case MyCons(x, xs) => aux(f(acc, x), xs)
    aux()

  // This is the same idea as foldLeftViaRight but the partial composition goes the other way.
  def foldRightViaLeft[B](z: B)(f: (A, B) => B): B =
    val func = this.foldLeft[B => B](identity[B]){ (acc, a) => acc.compose(b => f(a, b)) }
    func(z)

  def length: Int = foldLeft(0){ (acc, _) => 1 + acc }

  def reverse: MyList[A] = foldLeft(MyNil: MyList[A]) { (a, b) => MyCons(b, a) }

  def appendFoldRight[B >: A](other: MyList[B]): MyList[B] =
    foldRight(other) { (x, xs) => MyCons(x, xs) }

  def zip[B](other: MyList[B]): MyList[(A, B)] =
    // aux returns the list reverse-zipped
    @tailrec
    def aux(l1: MyList[A] = this, l2: MyList[B] = other, zipped: MyList[(A, B)] = MyNil): MyList[(A, B)] = (l1, l2) match
      case (MyNil, _) => zipped
      case (_, MyNil) => zipped
      case (MyCons(x, xs), MyCons(y, ys)) => aux(xs, ys, MyCons((x, y), zipped))
    aux().reverse

  // For tailrec methods, must be declared final.
  @tailrec
  final def startsWith[B >: A](prefix: MyList[B]): Boolean = (this, prefix) match
    case (_, MyNil) => true
    case (MyCons(x, xs), MyCons(y, ys)) if x == y => xs.startsWith(ys)
    case _ => false

  // For tailrec methods, must be declared final.
  @tailrec
  final def subsequence[B >: A](sub: MyList[B]): Boolean = this match
    case MyNil => sub == MyNil
    case _ if startsWith(sub) => true
    case MyCons(_, xs) => xs.subsequence(sub)

extension[A] (lsts: MyList[MyList[A]])
  def concatenate(): MyList[A] =
    lsts.foldRight(MyList.MyNil: MyList[A]){ (lst, acc) => lst.append(acc) }

// Sum and product require extensions.
extension (lst: MyList[Int])
  def sum: Int = lst.foldLeft(0){_ + _}
  def prod: Int = lst.foldLeft(1){_ * _}

extension (lst: MyList[Double])
  def sum: Double = lst.foldLeft(0.0){_ + _}
  def prod: Double = lst.foldLeft(1.0){_ * _}


object MyList:
  def apply[A](as: A*): MyList[A] =
    @tailrec
    def aux(rem: Seq[A] = as.reverse, acc: MyList[A] = MyNil): MyList[A] = rem match
      case Seq() => acc
      case x +: xs => aux(xs, MyCons(x, acc))
    aux()

  given [A: Showable]: Showable[MyList[A]] with
    extension (lst: MyList[A]) def show: String =
      lst.foldLeft(("[", true): (String, Boolean)){ case ((acc, first), a) =>
        (if first then acc + a.show else acc + ", " + a.show, false)
      }._1 + "]"

@main
def main_mylist(): Unit =
  import MyList.*
  val lst = MyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  println("check printing:")
  println(lst.show)
  println()

  println("reverse:")
  println(lst.reverse.show)
  println()

  println("setHead and multiple maps:")
  println(lst.setHead(99).map(_ * 10).map(_.toString.reverse).show)
  println()

  println("filter and reverse:")
  println(lst.filter(_ % 2 == 1).reverse.show)
  println()

  println("dropWhile:")
  println(lst.dropWhile(_ < 5).show)
  println()

  println("init:")
  println(lst.init.show)
  println()

  //  println("Calculating product...")
  //  println(product(MyList(1.0, 2.0, 0.0, 3.0, 4.0)))

  println("double list:")
  println(MyList(1.0, 2.0, 0.0, 3.0, 4.0).show)
  println()

  // This shows that all elements of the list are processed in foldRight, from right to left
  // via recursing to the end of the list and accumulating back up to the front, and thus all the
  // recursive calls must be unwound, and short-circuiting cannot happen.
  println("foldRight test:")
  println(MyList(1, 2, 3).foldRight(MyNil: MyList[Int], MyCons(_, _)))
  println()

  // Demonstrate that foldRight can result in a stack overflow.
  // Creating largeList succeeds, but the foldRight operation fails, whereas the foldLeft succeeds.
  // val bigThing = (0 to 10000000).map(_.toDouble)
  // val largeList = MyList(bigThing: _*)
  // println("Created list.")
  // System.out.flush()

  // This will cause a StackOverflowException.
  // val sum = largeList.foldRight(0, _ + _)
  // println(sum)

  // Confirm foldLeft using foldRight works.
  val smolList = MyList(1, 2, 3, 4)

  // We calculate the fold left of 2(2(2(2(0) + 1) + 2) + 3) + 4 = 26.
  println("foldLeftViaRight:")
  val weird1 = smolList.foldLeftViaRight(0){ (acc, a) => 2 * acc + a }
  println(s"2 * acc + x of $smolList = $weird1")
  println()

  // We calculate the fold right of 1 + 2(2 + 2(3 + 2(4 + 2(0)))) = 49.
  println("foldRightViaLeft:")
  val weird2 = smolList.foldRightViaLeft(0){ (a, acc) => a + 2 * acc }
  println(s"x + 2 * acc of $smolList = $weird2")
  println()

  // appendFoldRight
  println("appendFoldRight:")
  println(MyList(1, 2, 3).appendFoldRight(MyList(4, 5)).show)
  println()

  // Concatenation.
  println("concatenate:")
  println(MyList(MyList(1, 2, 3), MyList(4, 5), MyList(6, 7, 8, 9), MyList(10, 11), MyNil, MyList(12))
    .concatenate()
    .show)
  println()

  // flatMap
  println("flatMap:")
  println(f"map: ${smolList.map(a => MyList(a, a)).show}")
  println(f"map: ${smolList.flatMap(a => MyList(a, a)).show}")
  println()

  // zip
  println("zip:")
  val l1 = MyList(1, 2, 3)
  val l2 = MyList(4, 5, 6, 7)
  val l1zl2 = l1.zip(l2)
  println(f"zip ${l1.show} and ${l2.show} is ${l1zl2.show}")
  println(f"mapped with +: ${
    l1zl2.map {
      _ + _
    }.show
  }")
  println()

  // subsequence
  println("subsequence:")
  val s1 = MyList(3, 4, 6, 7, 9)
  val s2 = MyList(6, 7)
  val s3 = MyList(6, 7, 8)
  println(f"${s3.show} starts with ${s2.show}: ${s3.startsWith(s2)}")
  println(f"${s1.show} has subsequence ${s2.show}: ${s1.subsequence(s2)}")
  println(f"${s2.show} has subsequence ${s1.show}: ${s2.subsequence(s1)}")
  println()

  // Show that we can print a list of option.
  println("list of option:")
  val l_opt = MyList(MySome(1), MyNone, MySome(3))
  println(l_opt.show)
