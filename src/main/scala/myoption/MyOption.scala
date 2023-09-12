package org.vorpal
package myoption

import showable.Showable
import mylist.MyList
import mylist.MyList.*


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
//  def apply[A](a: A): MyOption[A] = MySome(a)
//  def apply(a: AnyRef) = a match
//    case null => MyNil
//    case _ => MySome(a)
  def apply[A](a: A): MyOption[A] = {
    if (a == null) MyNone
    else MySome(a)
  }

  def apply(a: Null): MyOption[Nothing] = MyNone

  given[A: Showable]: Showable[MyOption[A]] with
    extension (o: MyOption[A]) def show: String = o match
      case MySome(a) => s"MySome(${a.show})"
      case MyNone => "MyNone"

extension (xs: Seq[Double])
  def mean: MyOption[Double] =
    if xs.isEmpty then MyOption.MyNone
    else MyOption.MySome(xs.sum / xs.length)

  def variance: MyOption[Double] =
    xs.mean.flatMap(m => xs.map(x => math.pow(x - m, 2)).mean)

@main
def main_myoption(): Unit =
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
