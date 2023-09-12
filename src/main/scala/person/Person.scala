package org.vorpal
package person

import scala.annotation.tailrec

// Either of these will give the <= order for Ordering.
//import math.Ordered.orderingToOrdered
import scala.math.Ordering.Implicits.infixOrderingOps

def isSortedExplicit[A, F[_]](container: F[A], ordering: Ordering[A])(implicit ev: F[A] <:< Seq[A]): Boolean =
  isSorted(container)(ordering, ev)

def isSorted[A, F[_]](container: F[A])(implicit ordering: Ordering[A], ev: F[A] <:< Seq[A]): Boolean =
  @tailrec
  def aux(n: Int = 1): Boolean =
    n == container.length || (container(n - 1) <= container(n) && aux(n + 1))
  aux()

case class Person(first_name: String, last_name: String)

// personOrdering can be in the global scope, or in the Person object.
// Either way, it will be detected implicitly.
object Person:
  implicit val personOrdering: Ordering[Person] = Ordering.by { p => (p.last_name, p.first_name) }


@main
def main_person(): Unit =
  val p1 = Person("Andrew", "Coleman")
  val p2 = Person("Yagungle", "Supreme")
  val p3 = Person("Sebastian", "Raaphorst")
  val p4 = Person("Bruce", "Bothers")

  val people = List(p1, p2, p3, p4)
  println(s"$people is sorted? ${isSorted(people)}")

  // Note putting parentheses at the end of sorted causes issues detecting the implicit.
  val speople = people.sorted
  println(s"$speople is sorted? ${isSorted(speople)}")

  // Now reverse the sorting.
  val firstNameOrdering: Ordering[Person] = Ordering.by { p => (p.first_name, p.last_name) }
  val speople2 = people.sorted(firstNameOrdering) //p => (p.first_name, p.last_name) }
  println(s"$speople2 is sorted? ${isSortedExplicit(speople2, firstNameOrdering)}")
