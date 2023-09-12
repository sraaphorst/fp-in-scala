package org.vorpal
package myoption

import showable.Showable
import mylist.MyList
import mylist.MyList.*
import mylist.sum

enum MyOption[+A]:
  case MySome(get: A)
  case MyNone

  def map[B](f: A => B): MyOption[B] = this match
    case MySome(a) => MySome(f(a))
    case MyNone => MyNone

  def getOrElse[B >: A](default: => B): B = this match
    case MySome(a) => a
    case MyNone => default

  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    map(f).getOrElse(MyNone)

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(MySome(_)).getOrElse(ob)

  def filter(f: A => Boolean): MyOption[A] =
    flatMap(a => if f(a) then MySome(a) else MyNone)

object MyOption:
  def apply[A](a: A): MyOption[A] = {
    if (a == null) MyNone
    else MySome(a)
  }

  def apply(a: Null): MyOption[Nothing] = MyNone

  given[A: Showable]: Showable[MyOption[A]] with
    extension (o: MyOption[A]) def show: String = o match
      case MySome(a) => s"MySome(${a.show})"
      case MyNone => "MyNone"

  // Sequence the list, i.e. if any element of the list is MyNone, return MyNone.
  // This is included here because it is too generic to extend MyList[MyOption[A]].
  // Note that after we write traverse below, we can use it to rewrite sequence as we do below.
//  def sequence[A](lst: MyList[MyOption[A]]): MyOption[MyList[A]] =
//    lst.foldRight[MyOption[MyList[A]]](MySome(MyNil)) { (a, acc) =>
//      map2(a, acc)(MyCons.apply)
//    }
  def sequence[A](lst: MyList[MyOption[A]]): MyOption[MyList[A]] =
    lst.traverse(identity)

  // Now we write a function called map2 that takes a map of two parameters
  // and two MyOptions and produces a result if they're both defined.
  def map2[A, B, C](oa: MyOption[A], ob: MyOption[B])(f: (A, B) => C): MyOption[C] =
    oa.flatMap(a => ob.map(b => f(a, b)))

  // Note that map2 could be written in terms of a for comprehension, since for comprehensions
  // simply rely on flatMap and map calls. Here we implement map3 using this convention.
  def map3[A, B, C, D](oa: MyOption[A], ob: MyOption[B], oc: MyOption[C])(f: (A, B, C) => D): MyOption[D] =
    for
      a <- oa // flatMap
      b <- ob // flatMap
      c <- oc // map
    yield f(a, b, c)

  def mean(xs: MyList[Double]): MyOption[Double] =
    if xs.isEmpty then MyOption.MyNone
    else MyOption.MySome(xs.sum / xs.length)

  def variance(xs: MyList[Double]): MyOption[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  // Lift a function to an Option.
  def lift[A, B](f: A => B): Option[A] => Option[B] =
    _.map(f)

extension[A] (opt: MyOption[MyOption[A]])
  def flatten: MyOption[A] =
    opt.flatMap(identity)

// We can also write functions that accept multiple parameters where the parameters
// both have to be defined or the result is MyNone.
def performComputation(arg1: Int, arg2: Int): Int = ???

// Say we have data coming in from a webpage. We want to parse it to Int.
// Could use String.toIntOption but this shows some features of Scala pattern matching.
def toIntOption(input: String): MyOption[Int] =
  try MyOption.MySome(input.toInt)
  catch case _: NumberFormatException => MyOption.MyNone

extension[A] (lst: MyList[A])
  // Write traverse in terms of sequenceF.
  // Note that this is inefficient, because we have to do two passes over the list:
  // 1. First we call map on the list.
  // 2. Then we foldRight on the resultant list.
//   def traverse[B](f: A => MyOption[B]): MyOption[MyList[B]] =
//    MyOption.sequenceF(lst.map(f))

  // Write traverse as a fold expression.
  // This is much more efficient as we only iterate over the list once.
  def traverse[B](f: A => MyOption[B]): MyOption[MyList[B]] =
    lst.foldRight[MyOption[MyList[B]]](MyOption.MySome(MyList.MyNil)) { (a, acc) =>
      MyOption.map2(f(a), acc)(MyList.MyCons.apply)
    }

@main
def main_myoption(): Unit =
  import MyOption.*

  val o1 = MyOption.MySome(1)
  val o2: MyOption[Int] = MyOption.MyNone
  println(o1.show)
  println(o2.show)

  val o3 = MyOption(3)
  val o4: MyOption[String] = MyOption(null)
  println(o3.show)
  println(o4.show)

  val opt_list = MyOption(MyList("hello", "world"))
  println(opt_list.show)

  // Test out sequence.
  val list_opt1 = MyList(MySome(1), MySome(2), MySome(3), MySome(4))
  val list_opt2 = MyCons(MyNone, list_opt1)
  println(s"${list_opt1.show} => ${MyOption.sequence(list_opt1).show}")
  println(s"${list_opt2.show} => ${MyOption.sequence(list_opt2).show}")

  // Test some flattening.
  val f1: MyOption[MyOption[Int]] = MySome(MySome(2))
  val f2: MyOption[MyOption[Int]] = MySome(MyNone)
  val f3: MyOption[MyOption[Int]] = MyNone
  val f4: MyOption[MyOption[MyOption[Int]]] = MySome(MySome(MySome(3)))
  println(s"${f1.show}.flatten -> ${f1.flatten.show}")
  println(s"${f2.show}.flatten -> ${f2.flatten.show}")
  println(s"${f3.show}.flatten -> ${f3.flatten.show}")
  println(s"${f4.show}.flatten -> ${f4.flatten.show}")
