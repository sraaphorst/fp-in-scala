package org.vorpal
package showable

trait Showable[A]:
  extension (a: A) def show: String


object Showable:
  given Showable[Nothing] with
    extension (a: Nothing) def show: String = ""

  given Showable[Null] with
    extension (a: Null) def show: String = ""

  given Showable[Int] with
    extension (a: Int) def show: String = a.toString

  given Showable[Double] with
    extension (d: Double) def show: String = f"$d"

  given Showable[String] with
    extension (s: String) def show: String = s

  given[A: Showable, B: Showable]: Showable[(A, B)] with
    extension (pair: (A, B)) def show: String =
      val (a, b) = pair
      s"(${a.show}, ${b.show})"
